package High

import chisel3._
import chisel3.util._
import core._

class ToomCookParalBufferRdIO(dataWidth: Int, addrWidth: Int, parallel: Int)
    extends Bundle {
  val en = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val dout = Output(Vec(parallel, UInt(dataWidth.W)))
}

class ToomCookParalBufferWrIO(dataWidth: Int, addrWidth: Int, parallel: Int)
    extends Bundle {
  val we = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val din = Input(Vec(parallel, UInt(dataWidth.W)))
}

class ToomCookSpBufferRWIO(dataWidth: Int, addrWidth: Int) extends Bundle {
  val en = Input(Bool())
  val we = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val din = Input(UInt(dataWidth.W))
  val dout = Output(UInt(dataWidth.W))
}

class ToomCook1024ExternalIO extends Bundle {
  val start = Input(Bool())
  val busy = Output(Bool())
  val done = Output(Bool())

  val aMem =
    Vec(4, Flipped(new ToomCookParalBufferRdIO(24, 4, 16)))
  val bMem =
    Vec(4, Flipped(new ToomCookParalBufferRdIO(8, 4, 16)))

  val resultReady = Input(Bool())
  val cMem =
    Vec(4, Flipped(new ToomCookParalBufferWrIO(24, 4, 16)))
}

class ToomCook1024CoreIO(evalPairWidth: Int) extends Bundle {
  val inf = new ToomCook1024ExternalIO

  // E0 is reused once per outer group.
  val e0Scratch =
    Flipped(new ToomCookSpBufferRWIO(evalPairWidth, 5))

  // Eval->Core stores E1, not the final E2 values.
  val e1Store = Vec(
    2,
    Flipped(new ToomCookSpBufferRWIO(evalPairWidth, 8))
  )

  // Core->Inte stores the first interpolation result I0.
  val i0Store = Vec(
    2,
    Flipped(new ToomCookSpBufferRWIO(16 * 33, 8))
  )

  // I1->I2 is private to the interpolation stage.
  val i1Store =
    Flipped(new ToomCookSpBufferRWIO(16 * 27, 7))
}

/** Compatibility IO used by ToomCook1024WithSram and existing tests. */
class ToomCook1024IO extends Bundle {
  val start = Input(Bool())
  val busy = Output(Bool())
  val done = Output(Bool())
  val a_we = Input(Bool())
  val a_addr = Input(UInt(4.W))
  val a_din = Input(UInt((64 * 24).W))
  val b_we = Input(Bool())
  val b_addr = Input(UInt(4.W))
  val b_din = Input(UInt((64 * 8).W))
  val c_re = Input(Bool())
  val c_addr = Input(UInt(4.W))
  val c_dout = Output(UInt((64 * 24).W))
  val c_valid = Output(Bool())
}

/**
  * Three-slot striding Toom-Cook pipeline with narrow physical SRAMs.
  *
  * Eval executes E0/E1 and stores E1.  The Core input adapter evaluates E2
  * while feeding the unchanged core16.  The Core output adapter immediately
  * executes I0.  Inte therefore executes only I1/I2.
  */
class ToomCook1024Core(
    t: Int = 0,
    k: Int = 2,
    sign: Int = 1,
    aEvalWidth: Int = EvalWidth.A_EVAL_W,
    bEvalWidth: Int = EvalWidth.B_EVAL_W,
    coreOutWidth: Int = 36
) extends Module {
  require(aEvalWidth == EvalWidth.A_EVAL_W)
  require(bEvalWidth == EvalWidth.B_EVAL_W)
  require(coreOutWidth == 36)

  private val A_EVAL_W = aEvalWidth
  private val B_EVAL_W = bEvalWidth
  private val A_WORD_W = 16 * A_EVAL_W
  private val B_WORD_W = 16 * B_EVAL_W
  private val EVAL_PAIR_W = A_WORD_W + B_WORD_W
  private val CORE_WORD_W = 16 * coreOutWidth
  private val I0_WORD_W = 16 * 33
  private val I1_WORD_W = 16 * 27

  val io = IO(new ToomCook1024CoreIO(EVAL_PAIR_W))
  val inf = io.inf

  private def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)

  private def unpackVec(x: UInt, count: Int, width: Int): Vec[UInt] = {
    val out = Wire(Vec(count, UInt(width.W)))
    for (index <- 0 until count) {
      out(index) := x((index + 1) * width - 1, index * width)
    }
    out
  }

  private def split16(x: UInt, width: Int): Vec[UInt] =
    unpackVec(x, 16, width)

  private def pack16(xs: Seq[UInt]): UInt = {
    require(xs.length == 16)
    packVec(xs)
  }

  private def clearRamPort(port: ToomCookSpBufferRWIO): Unit = {
    port.en := false.B
    port.we := false.B
    port.addr := 0.U.asTypeOf(port.addr)
    port.din := 0.U.asTypeOf(port.din)
  }

  clearRamPort(io.e0Scratch)
  clearRamPort(io.i1Store)
  for (slot <- 0 until 2) {
    clearRamPort(io.e1Store(slot))
    clearRamPort(io.i0Store(slot))
  }

  val doneReg = RegInit(false.B)
  inf.done := doneReg
  doneReg := false.B

  for (bank <- 0 until 4) {
    inf.aMem(bank).en := false.B
    inf.aMem(bank).addr := 0.U
    inf.bMem(bank).en := false.B
    inf.bMem(bank).addr := 0.U
    inf.cMem(bank).we := false.B
    inf.cMem(bank).addr := 0.U
    inf.cMem(bank).din := VecInit(Seq.fill(16)(0.U(24.W)))
  }

  val memEmpty = 0.U(2.W)
  val memWriting = 1.U(2.W)
  val memReady = 2.U(2.W)
  val memReading = 3.U(2.W)

  val e1State = RegInit(VecInit(Seq.fill(2)(memEmpty)))
  val i0State = RegInit(VecInit(Seq.fill(2)(memEmpty)))

  val evalWritePtr = RegInit(false.B)
  val coreReadPtr = RegInit(false.B)
  val coreWritePtr = RegInit(false.B)
  val inteReadPtr = RegInit(false.B)

  val inputFull = RegInit(false.B)
  inf.busy := inputFull
  when(inf.start && !inputFull) {
    inputFull := true.B
  }

  // ------------------------------------------------------------------------
  // Eval slot: E0/E1.  Seven parallel Eval outputs are drained one per cycle.
  // ------------------------------------------------------------------------
  val sharedEvalA =
    Seq.fill(16)(Module(new Eval(A_EVAL_W, A_EVAL_W)))
  val sharedEvalB =
    Seq.fill(16)(Module(new Eval(B_EVAL_W, B_EVAL_W)))

  val evalWindowA = Reg(Vec(3, UInt(A_WORD_W.W)))
  val evalWindowB = Reg(Vec(3, UInt(B_WORD_W.W)))
  val evalPending = Reg(Vec(7, UInt(EVAL_PAIR_W.W)))

  val Seq(
    eIdle,
    e0Issue,
    e0Wait,
    e0Write,
    e1Prime,
    e1PrimeWait,
    e1Write
  ) = Enum(7)
  val evalState = RegInit(eIdle)

  val evalOutSlot = RegInit(false.B)
  val evalOuter = RegInit(0.U(2.W))
  val e0Inner = RegInit(0.U(2.W))
  val e0WritePoint = RegInit(0.U(3.W))
  val e1Pt0 = RegInit(0.U(3.W))
  val e1PrimePart = RegInit(0.U(2.W))
  val e1WritePoint = RegInit(0.U(3.W))

  val e0ReadFire =
    evalState === e0Issue ||
      (evalState === e0Write &&
        e0WritePoint === 5.U && e0Inner =/= 3.U)
  val e0ReadValid = RegNext(e0ReadFire, false.B)
  val e0ReadInnerD = RegEnable(
    Mux(evalState === e0Issue, e0Inner, e0Inner + 1.U),
    e0ReadFire
  )
  val e0ReadOuterD = RegEnable(evalOuter, e0ReadFire)

  when(e0ReadFire) {
    val readInner =
      Mux(evalState === e0Issue, e0Inner, e0Inner + 1.U)
    for (bank <- 0 until 4) {
      inf.aMem(bank).en := true.B
      inf.aMem(bank).addr := Cat(evalOuter, readInner)
      inf.bMem(bank).en := true.B
      inf.bMem(bank).addr := Cat(evalOuter, readInner)
    }
    when(evalState === e0Issue) {
      evalState := e0Wait
    }
  }

  val e1ReadFire =
    evalState === e1Prime ||
      (evalState === e1Write &&
        e1WritePoint >= 2.U && e1WritePoint <= 5.U &&
        e1Pt0 =/= 6.U)
  val e1ReadValid = RegNext(e1ReadFire, false.B)
  val e1ReadPart = Wire(UInt(2.W))
  val e1ReadPt0 = Wire(UInt(3.W))
  e1ReadPart := e1PrimePart
  e1ReadPt0 := e1Pt0
  when(evalState === e1Write) {
    e1ReadPart := e1WritePoint - 2.U
    e1ReadPt0 := e1Pt0 + 1.U
  }
  val e1ReadPartD = RegEnable(e1ReadPart, e1ReadFire)
  val e1ReadPt0D = RegEnable(e1ReadPt0, e1ReadFire)

  when(e1ReadFire) {
    io.e0Scratch.en := true.B
    io.e0Scratch.addr := e1ReadPt0 * 4.U + e1ReadPart
    when(evalState === e1Prime) {
      when(e1PrimePart === 3.U) {
        evalState := e1PrimeWait
      }.otherwise {
        e1PrimePart := e1PrimePart + 1.U
      }
    }
  }

  val scratchA = io.e0Scratch.dout(A_WORD_W - 1, 0)
  val scratchB = io.e0Scratch.dout(EVAL_PAIR_W - 1, A_WORD_W)

  for (lane <- 0 until 16; part <- 0 until 4) {
    val highAWord =
      if (part < 3) evalWindowA(part) else scratchA
    val highBWord =
      if (part < 3) evalWindowB(part) else scratchB
    val highA =
      highAWord((lane + 1) * A_EVAL_W - 1, lane * A_EVAL_W)
    val highB =
      highBWord((lane + 1) * B_EVAL_W - 1, lane * B_EVAL_W)

    sharedEvalA(lane).io.in(part) :=
      Mux(e0ReadValid, inf.aMem(part).dout(lane), highA)
    sharedEvalB(lane).io.in(part) :=
      Mux(e0ReadValid, inf.bMem(part).dout(lane), highB)
  }

  val evalPairOut = Wire(Vec(7, UInt(EVAL_PAIR_W.W)))
  for (point <- 0 until 7) {
    val aWord =
      pack16((0 until 16).map(lane => sharedEvalA(lane).io.out(point)))
    val bWord =
      pack16((0 until 16).map(lane => sharedEvalB(lane).io.out(point)))
    evalPairOut(point) := Cat(bWord, aWord)
  }

  when(e0ReadValid) {
    for (point <- 0 until 7) {
      evalPending(point) := evalPairOut(point)
    }
    when(evalState === e0Wait) {
      e0WritePoint := 0.U
      evalState := e0Write
    }
    when(e0ReadOuterD === 3.U && e0ReadInnerD === 3.U) {
      inputFull := false.B
    }
  }

  when(evalState === e0Write) {
    io.e0Scratch.en := true.B
    io.e0Scratch.we := true.B
    io.e0Scratch.addr := e0WritePoint * 4.U + e0Inner
    io.e0Scratch.din := evalPending(e0WritePoint)

    when(e0WritePoint === 6.U) {
      when(e0ReadValid) {
        e0Inner := e0ReadInnerD
        e0WritePoint := 0.U
      }.otherwise {
        e1Pt0 := 0.U
        e1PrimePart := 0.U
        evalState := e1Prime
      }
    }.otherwise {
      e0WritePoint := e0WritePoint + 1.U
    }
  }

  when(e1ReadValid && e1ReadPartD =/= 3.U) {
    evalWindowA(e1ReadPartD) := scratchA
    evalWindowB(e1ReadPartD) := scratchB
  }

  when(e1ReadValid && e1ReadPartD === 3.U) {
    for (point <- 0 until 7) {
      evalPending(point) := evalPairOut(point)
    }
    when(evalState === e1PrimeWait) {
      e1Pt0 := e1ReadPt0D
      e1WritePoint := 0.U
      evalState := e1Write
    }
  }

  when(evalState === e1Write) {
    for (slot <- 0 until 2) {
      when(evalOutSlot === slot.U) {
        io.e1Store(slot).en := true.B
        io.e1Store(slot).we := true.B
        io.e1Store(slot).addr :=
          e1WritePoint * 28.U + e1Pt0 * 4.U + evalOuter
        io.e1Store(slot).din := evalPending(e1WritePoint)
      }
    }

    when(e1WritePoint === 6.U) {
      when(e1Pt0 =/= 6.U) {
        assert(
          e1ReadValid && e1ReadPartD === 3.U,
          "E1 prefetch did not finish before output drain"
        )
        e1Pt0 := e1Pt0 + 1.U
        e1WritePoint := 0.U
      }.otherwise {
        when(evalOuter === 3.U) {
          e1State(evalOutSlot) := memReady
          evalState := eIdle
        }.otherwise {
          evalOuter := evalOuter + 1.U
          e0Inner := 0.U
          evalState := e0Issue
        }
      }
    }.otherwise {
      e1WritePoint := e1WritePoint + 1.U
    }
  }

  when(
    evalState === eIdle && inputFull &&
      e1State(evalWritePtr) === memEmpty
  ) {
    evalOutSlot := evalWritePtr
    e1State(evalWritePtr) := memWriting
    evalWritePtr := ~evalWritePtr
    evalOuter := 0.U
    e0Inner := 0.U
    e0WritePoint := 0.U
    evalState := e0Issue
  }

  // ------------------------------------------------------------------------
  // Core slot: point-select E2 adapter -> unchanged core16 -> immediate I0.
  // ------------------------------------------------------------------------
  val core = Module(new core16(
    t = t,
    k = k,
    sign = sign,
    aWidth = A_EVAL_W,
    bWidth = B_EVAL_W,
    cWidth = coreOutWidth
  ))

  val coreInput = Reg(Vec(2, Vec(4, UInt(EVAL_PAIR_W.W))))
  val coreCurSel = RegInit(false.B)

  val pointEvalA =
    Seq.fill(16)(Module(new EvalPoint(A_EVAL_W, A_EVAL_W)))
  val pointEvalB =
    Seq.fill(16)(Module(new EvalPoint(B_EVAL_W, B_EVAL_W)))

  val cIdle :: cPrime :: cPrimeWait :: cRun :: Nil = Enum(4)
  val coreState = RegInit(cIdle)
  val coreInSlot = RegInit(false.B)
  val coreOutSlot = RegInit(false.B)
  val corePrimePart = RegInit(0.U(2.W))
  val coreGroup = RegInit(0.U(6.W))
  val corePt0 = RegInit(0.U(3.W))
  val corePt1 = RegInit(0.U(3.W))
  val corePt2 = RegInit(0.U(3.W))

  val coreChainValid = RegInit(false.B)
  val coreChainInSlot = RegInit(false.B)
  val coreChainOutSlot = RegInit(false.B)

  val corePrimeRead = coreState === cPrime
  val corePrefetchRead =
    coreState === cRun && corePt2 >= 1.U && corePt2 <= 4.U &&
      (coreGroup =/= 48.U || coreChainValid)
  val coreE1ReadFire = corePrimeRead || corePrefetchRead

  val coreReadPart = Wire(UInt(2.W))
  val coreReadSlot = Wire(Bool())
  val coreReadAddr = Wire(UInt(8.W))
  val coreReadCaptureSel = Wire(Bool())

  coreReadPart := corePrimePart
  coreReadSlot := coreInSlot
  coreReadAddr := corePrimePart
  coreReadCaptureSel := false.B

  when(corePrefetchRead) {
    coreReadPart := corePt2 - 1.U
    coreReadCaptureSel := !coreCurSel
    when(coreGroup === 48.U) {
      coreReadSlot := coreChainInSlot
      coreReadAddr := corePt2 - 1.U
    }.otherwise {
      val nextPt0 =
        Mux(corePt1 === 6.U, corePt0 + 1.U, corePt0)
      val nextPt1 =
        Mux(corePt1 === 6.U, 0.U, corePt1 + 1.U)
      coreReadSlot := coreInSlot
      coreReadAddr :=
        nextPt1 * 28.U + nextPt0 * 4.U + (corePt2 - 1.U)
    }
  }

  when(coreE1ReadFire) {
    for (slot <- 0 until 2) {
      when(coreReadSlot === slot.U) {
        io.e1Store(slot).en := true.B
        io.e1Store(slot).addr := coreReadAddr
      }
    }
    when(corePrimeRead) {
      when(corePrimePart === 3.U) {
        coreState := cPrimeWait
      }.otherwise {
        corePrimePart := corePrimePart + 1.U
      }
    }
  }

  val coreE1ReadValid = RegNext(coreE1ReadFire, false.B)
  val coreReadPartD = RegEnable(coreReadPart, coreE1ReadFire)
  val coreReadSlotD = RegEnable(coreReadSlot, coreE1ReadFire)
  val coreReadCaptureSelD =
    RegEnable(coreReadCaptureSel, coreE1ReadFire)

  val coreReadWord = Mux(
    coreReadSlotD,
    io.e1Store(1).dout,
    io.e1Store(0).dout
  )

  when(coreE1ReadValid) {
    coreInput(coreReadCaptureSelD)(coreReadPartD) := coreReadWord
    when(coreState === cPrimeWait && coreReadPartD === 3.U) {
      coreCurSel := false.B
      coreGroup := 0.U
      corePt0 := 0.U
      corePt1 := 0.U
      corePt2 := 0.U
      coreState := cRun
    }
  }

  for (lane <- 0 until 16) {
    for (part <- 0 until 4) {
      val pair = Mux(
        coreCurSel,
        coreInput(1)(part),
        coreInput(0)(part)
      )
      val aWord = pair(A_WORD_W - 1, 0)
      val bWord = pair(EVAL_PAIR_W - 1, A_WORD_W)
      pointEvalA(lane).io.r(part) :=
        aWord((lane + 1) * A_EVAL_W - 1, lane * A_EVAL_W)
      pointEvalB(lane).io.r(part) :=
        bWord((lane + 1) * B_EVAL_W - 1, lane * B_EVAL_W)
    }
    pointEvalA(lane).io.pt := corePt2
    pointEvalB(lane).io.pt := corePt2
    core.io.a(lane) := pointEvalA(lane).io.out
    core.io.b(lane) := pointEvalB(lane).io.out
  }

  val coreInputValid = coreState === cRun
  core.io.valid_in := coreInputValid

  when(
    coreState === cRun && coreGroup === 48.U && corePt2 === 0.U &&
      e1State(coreReadPtr) === memReady &&
      i0State(coreWritePtr) === memEmpty
  ) {
    coreChainValid := true.B
    coreChainInSlot := coreReadPtr
    coreChainOutSlot := coreWritePtr
    e1State(coreReadPtr) := memReading
    i0State(coreWritePtr) := memWriting
    coreReadPtr := ~coreReadPtr
    coreWritePtr := ~coreWritePtr
  }

  when(coreState === cRun) {
    when(corePt2 === 6.U) {
      corePt2 := 0.U
      when(coreGroup === 48.U) {
        e1State(coreInSlot) := memEmpty
        when(coreChainValid) {
          coreInSlot := coreChainInSlot
          coreOutSlot := coreChainOutSlot
          coreCurSel := !coreCurSel
          coreGroup := 0.U
          corePt0 := 0.U
          corePt1 := 0.U
          coreChainValid := false.B
        }.otherwise {
          coreState := cIdle
        }
      }.otherwise {
        coreCurSel := !coreCurSel
        coreGroup := coreGroup + 1.U
        when(corePt1 === 6.U) {
          corePt1 := 0.U
          corePt0 := corePt0 + 1.U
        }.otherwise {
          corePt1 := corePt1 + 1.U
        }
      }
    }.otherwise {
      corePt2 := corePt2 + 1.U
    }
  }

  when(
    coreState === cIdle &&
      e1State(coreReadPtr) === memReady &&
      i0State(coreWritePtr) === memEmpty
  ) {
    coreInSlot := coreReadPtr
    coreOutSlot := coreWritePtr
    e1State(coreReadPtr) := memReading
    i0State(coreWritePtr) := memWriting
    coreReadPtr := ~coreReadPtr
    coreWritePtr := ~coreWritePtr
    corePrimePart := 0.U
    coreChainValid := false.B
    coreState := cPrime
  }

  val coreMetaValid =
    ShiftRegister(coreInputValid, 4, false.B, true.B)
  val coreMetaGroup = ShiftRegister(coreGroup, 4)
  val coreMetaPoint = ShiftRegister(corePt2, 4)
  val coreMetaSlot = ShiftRegister(coreOutSlot, 4)
  val coreOutWord =
    pack16((0 until 16).map(index => core.io.c(index)))

  when(core.io.valid_out || coreMetaValid) {
    assert(
      core.io.valid_out === coreMetaValid,
      "Core latency changed: update High metadata delay"
    )
  }

  val coreResultSet = Reg(Vec(6, UInt(CORE_WORD_W.W)))
  when(core.io.valid_out && coreMetaValid && coreMetaPoint =/= 6.U) {
    coreResultSet(coreMetaPoint) := coreOutWord
  }

  val interpCore = Module(new Interp16ColsShared)
  interpCore.io.mode := 0.U
  interpCore.io.pr0 := 0.U
  interpCore.io.pr1 := 0.U
  interpCore.io.pr2 := 0.U
  for (point <- 0 until 7; lane <- 0 until 16) {
    val word =
      if (point < 6) coreResultSet(point) else coreOutWord
    interpCore.io.in(point * 16 + lane) :=
      word((lane + 1) * coreOutWidth - 1, lane * coreOutWidth)
  }

  private def interpChunk(
      out: Vec[UInt],
      chunk: Int,
      width: Int
  ): UInt = {
    pack16((0 until 16).map { lane =>
      out(chunk * 16 + lane)(width - 1, 0)
    })
  }

  val i0Raw = Wire(Vec(4, UInt(I0_WORD_W.W)))
  for (chunk <- 0 until 4) {
    i0Raw(chunk) := interpChunk(interpCore.io.out, chunk, 33)
  }
  val i0First = split16(i0Raw(0), 33)
  val i0Corrected = Wire(Vec(16, UInt(33.W)))
  i0Corrected := i0First
  i0Corrected(0) :=
    ParaMath.mask(i0First(0) - interpCore.io.nr2, 33)
  i0Corrected(1) :=
    ParaMath.mask(i0First(1) - interpCore.io.nr1, 33)
  i0Corrected(2) :=
    ParaMath.mask(i0First(2) - interpCore.io.nr0, 33)

  val i0Pending = Reg(Vec(4, UInt(I0_WORD_W.W)))
  val i0PendingGroup = Reg(UInt(6.W))
  val i0PendingSlot = Reg(Bool())
  val i0WriteValid = RegInit(false.B)
  val i0WriteIndex = RegInit(0.U(2.W))

  when(core.io.valid_out && coreMetaValid && coreMetaPoint === 6.U) {
    i0Pending(0) := pack16(i0Corrected)
    for (chunk <- 1 until 4) {
      i0Pending(chunk) := i0Raw(chunk)
    }
    i0PendingGroup := coreMetaGroup
    i0PendingSlot := coreMetaSlot
    i0WriteValid := true.B
    i0WriteIndex := 0.U
  }

  when(i0WriteValid) {
    for (slot <- 0 until 2) {
      when(i0PendingSlot === slot.U) {
        io.i0Store(slot).en := true.B
        io.i0Store(slot).we := true.B
        io.i0Store(slot).addr :=
          i0PendingGroup * 4.U + i0WriteIndex
        io.i0Store(slot).din := i0Pending(i0WriteIndex)
      }
    }
    when(i0WriteIndex === 3.U) {
      i0WriteValid := false.B
      when(i0PendingGroup === 48.U) {
        i0State(i0PendingSlot) := memReady
      }
    }.otherwise {
      i0WriteIndex := i0WriteIndex + 1.U
    }
  }

  // ------------------------------------------------------------------------
  // Inte slot: sequentially gather seven point words, then execute I1/I2.
  // ------------------------------------------------------------------------
  val interpInte = Module(new Interp16ColsShared)
  interpInte.io.mode := 1.U
  interpInte.io.in := VecInit(Seq.fill(7 * 16)(0.U(36.W)))
  interpInte.io.pr0 := 0.U
  interpInte.io.pr1 := 0.U
  interpInte.io.pr2 := 0.U

  val iIdle :: iI1 :: iI2 :: iI2Patch :: Nil = Enum(4)
  val inteState = RegInit(iIdle)
  val inteInSlot = RegInit(false.B)

  val i1IssueP0 = RegInit(0.U(3.W))
  val i1IssueChunk = RegInit(0.U(2.W))
  val i1IssuePoint = RegInit(0.U(3.W))
  val i1IssueDone = RegInit(false.B)
  val i1Gather = Reg(Vec(6, UInt(I0_WORD_W.W)))
  val i1Carry0 = RegInit(0.U(27.W))
  val i1Carry1 = RegInit(0.U(27.W))
  val i1Carry2 = RegInit(0.U(27.W))
  val i1FirstWord = Reg(UInt(I1_WORD_W.W))

  val i1ReadFire = inteState === iI1 && !i1IssueDone
  val i1ReadValid = RegNext(i1ReadFire, false.B)
  val i1ReadP0D = RegEnable(i1IssueP0, i1ReadFire)
  val i1ReadChunkD = RegEnable(i1IssueChunk, i1ReadFire)
  val i1ReadPointD = RegEnable(i1IssuePoint, i1ReadFire)

  when(i1ReadFire) {
    val readAddr =
      (i1IssueP0 * 7.U + i1IssuePoint) * 4.U + i1IssueChunk
    for (slot <- 0 until 2) {
      when(inteInSlot === slot.U) {
        io.i0Store(slot).en := true.B
        io.i0Store(slot).addr := readAddr
      }
    }

    when(i1IssuePoint === 6.U) {
      i1IssuePoint := 0.U
      when(i1IssueChunk === 3.U) {
        i1IssueChunk := 0.U
        when(i1IssueP0 === 6.U) {
          i1IssueDone := true.B
        }.otherwise {
          i1IssueP0 := i1IssueP0 + 1.U
        }
      }.otherwise {
        i1IssueChunk := i1IssueChunk + 1.U
      }
    }.otherwise {
      i1IssuePoint := i1IssuePoint + 1.U
    }
  }

  val i1ReadWord = Mux(
    inteInSlot,
    io.i0Store(1).dout,
    io.i0Store(0).dout
  )
  when(i1ReadValid && i1ReadPointD =/= 6.U) {
    i1Gather(i1ReadPointD) := i1ReadWord
  }

  when(inteState === iI1) {
    interpInte.io.mode := 1.U
    interpInte.io.pr0 :=
      Mux(i1ReadChunkD === 0.U, 0.U, i1Carry0)
    interpInte.io.pr1 :=
      Mux(i1ReadChunkD === 0.U, 0.U, i1Carry1)
    interpInte.io.pr2 :=
      Mux(i1ReadChunkD === 0.U, 0.U, i1Carry2)
    for (point <- 0 until 7; lane <- 0 until 16) {
      val word =
        if (point < 6) i1Gather(point) else i1ReadWord
      interpInte.io.in(point * 16 + lane) :=
        word((lane + 1) * 33 - 1, lane * 33)
    }
  }

  val i1Raw = Wire(Vec(4, UInt(I1_WORD_W.W)))
  for (chunk <- 0 until 4) {
    i1Raw(chunk) := interpChunk(interpInte.io.out, chunk, 27)
  }

  val i1FirstVec = split16(i1FirstWord, 27)
  val i1Corrected = Wire(Vec(16, UInt(27.W)))
  i1Corrected := i1FirstVec
  i1Corrected(0) :=
    ParaMath.mask(i1FirstVec(0) - interpInte.io.nr2(26, 0), 27)
  i1Corrected(1) :=
    ParaMath.mask(i1FirstVec(1) - interpInte.io.nr1(26, 0), 27)
  i1Corrected(2) :=
    ParaMath.mask(i1FirstVec(2) - interpInte.io.nr0(26, 0), 27)

  val i1Output = Reg(Vec(4, UInt(I1_WORD_W.W)))
  val i1PatchWord = Reg(UInt(I1_WORD_W.W))
  val i1OutputP0 = Reg(UInt(3.W))
  val i1OutputChunk = Reg(UInt(2.W))
  val i1OutputNeedsPatch = RegInit(false.B)
  val i1WriteValid = RegInit(false.B)
  val i1WriteIndex = RegInit(0.U(3.W))

  when(i1ReadValid && i1ReadPointD === 6.U) {
    for (chunk <- 0 until 4) {
      i1Output(chunk) := i1Raw(chunk)
    }
    i1OutputP0 := i1ReadP0D
    i1OutputChunk := i1ReadChunkD
    i1OutputNeedsPatch := i1ReadChunkD === 3.U
    i1WriteValid := true.B
    i1WriteIndex := 0.U

    i1Carry0 := interpInte.io.nr0(26, 0)
    i1Carry1 := interpInte.io.nr1(26, 0)
    i1Carry2 := interpInte.io.nr2(26, 0)
    when(i1ReadChunkD === 0.U) {
      i1FirstWord := i1Raw(0)
    }
    when(i1ReadChunkD === 3.U) {
      i1PatchWord := pack16(i1Corrected)
    }
    when(i1ReadP0D === 6.U && i1ReadChunkD === 3.U) {
      i0State(inteInSlot) := memEmpty
    }
  }

  when(i1WriteValid) {
    io.i1Store.en := true.B
    io.i1Store.we := true.B
    when(i1WriteIndex < 4.U) {
      io.i1Store.addr :=
        i1OutputP0 * 16.U +
          i1OutputChunk * 4.U + i1WriteIndex
      io.i1Store.din := i1Output(i1WriteIndex)
      when(i1WriteIndex === 3.U) {
        when(i1OutputNeedsPatch) {
          i1WriteIndex := 4.U
        }.otherwise {
          i1WriteValid := false.B
        }
      }.otherwise {
        i1WriteIndex := i1WriteIndex + 1.U
      }
    }.otherwise {
      io.i1Store.addr := i1OutputP0 * 16.U
      io.i1Store.din := i1PatchWord
      i1WriteValid := false.B
      when(i1OutputP0 === 6.U) {
        inteState := iI2
      }
    }
  }

  val i2IssueChunk = RegInit(0.U(4.W))
  val i2IssuePoint = RegInit(0.U(3.W))
  val i2IssueDone = RegInit(false.B)
  val i2Gather = Reg(Vec(6, UInt(I1_WORD_W.W)))
  val i2Carry0 = RegInit(0.U(24.W))
  val i2Carry1 = RegInit(0.U(24.W))
  val i2Carry2 = RegInit(0.U(24.W))
  val i2FirstWord = Reg(UInt((16 * 24).W))
  val i2PatchWord = Reg(UInt((16 * 24).W))

  val i2ReadFire = inteState === iI2 && !i2IssueDone
  val i2ReadValid = RegNext(i2ReadFire, false.B)
  val i2ReadChunkD = RegEnable(i2IssueChunk, i2ReadFire)
  val i2ReadPointD = RegEnable(i2IssuePoint, i2ReadFire)

  when(i2ReadFire) {
    io.i1Store.en := true.B
    io.i1Store.addr := i2IssuePoint * 16.U + i2IssueChunk
    when(i2IssuePoint === 6.U) {
      i2IssuePoint := 0.U
      when(i2IssueChunk === 15.U) {
        i2IssueDone := true.B
      }.otherwise {
        i2IssueChunk := i2IssueChunk + 1.U
      }
    }.otherwise {
      i2IssuePoint := i2IssuePoint + 1.U
    }
  }

  when(i2ReadValid && i2ReadPointD =/= 6.U) {
    i2Gather(i2ReadPointD) := io.i1Store.dout
  }

  when(inteState === iI2 || inteState === iI2Patch) {
    interpInte.io.mode := 2.U
    interpInte.io.pr0 := i2Carry0
    interpInte.io.pr1 := i2Carry1
    interpInte.io.pr2 := i2Carry2
    for (point <- 0 until 7; lane <- 0 until 16) {
      val word =
        if (point < 6) i2Gather(point) else io.i1Store.dout
      interpInte.io.in(point * 16 + lane) :=
        word((lane + 1) * 27 - 1, lane * 27)
    }
  }

  val i2Raw = Wire(Vec(4, UInt((16 * 24).W)))
  for (chunk <- 0 until 4) {
    i2Raw(chunk) := interpChunk(interpInte.io.out, chunk, 24)
  }

  when(i2ReadValid && i2ReadPointD === 6.U) {
    for (chunk <- 0 until 4) {
      inf.cMem(chunk).we := true.B
      inf.cMem(chunk).addr := i2ReadChunkD
      inf.cMem(chunk).din := split16(i2Raw(chunk), 24)
    }
    when(i2ReadChunkD === 0.U) {
      i2FirstWord := i2Raw(0)
    }
    i2Carry0 := interpInte.io.nr0(23, 0)
    i2Carry1 := interpInte.io.nr1(23, 0)
    i2Carry2 := interpInte.io.nr2(23, 0)

    when(i2ReadChunkD === 15.U) {
      val first = split16(i2FirstWord, 24)
      val corrected = Wire(Vec(16, UInt(24.W)))
      corrected := first
      corrected(0) :=
        ParaMath.mask(first(0) - interpInte.io.nr2(23, 0), 24)
      corrected(1) :=
        ParaMath.mask(first(1) - interpInte.io.nr1(23, 0), 24)
      corrected(2) :=
        ParaMath.mask(first(2) - interpInte.io.nr0(23, 0), 24)
      i2PatchWord := pack16(corrected)
      inteState := iI2Patch
    }
  }

  when(inteState === iI2Patch) {
    inf.cMem(0).we := true.B
    inf.cMem(0).addr := 0.U
    inf.cMem(0).din := split16(i2PatchWord, 24)
    doneReg := true.B
    inteState := iIdle
  }

  when(
    inteState === iIdle &&
      i0State(inteReadPtr) === memReady &&
      inf.resultReady
  ) {
    inteInSlot := inteReadPtr
    i0State(inteReadPtr) := memReading
    inteReadPtr := ~inteReadPtr

    i1IssueP0 := 0.U
    i1IssueChunk := 0.U
    i1IssuePoint := 0.U
    i1IssueDone := false.B
    i1Carry0 := 0.U
    i1Carry1 := 0.U
    i1Carry2 := 0.U
    i1WriteValid := false.B

    i2IssueChunk := 0.U
    i2IssuePoint := 0.U
    i2IssueDone := false.B
    i2Carry0 := 0.U
    i2Carry1 := 0.U
    i2Carry2 := 0.U

    inteState := iI1
  }
}

/**
  * Memory-backed top.  External input/output SRAM remains outside this module.
  */
class ToomCook1024(
    t: Int = 0,
    k: Int = 2,
    sign: Int = 1,
    aEvalWidth: Int = EvalWidth.A_EVAL_W,
    bEvalWidth: Int = EvalWidth.B_EVAL_W,
    coreOutWidth: Int = 36,
    useMemoryCompiler: Boolean = true
) extends Module {
  val io = IO(new ToomCook1024ExternalIO)

  private val aWordWidth = 16 * aEvalWidth
  private val bWordWidth = 16 * bEvalWidth
  private val evalPairWidth = aWordWidth + bWordWidth
  val arithmetic = Module(new ToomCook1024Core(
    t = t,
    k = k,
    sign = sign,
    aEvalWidth = aEvalWidth,
    bEvalWidth = bEvalWidth,
    coreOutWidth = coreOutWidth
  ))

  arithmetic.io.inf.start := io.start
  io.busy := arithmetic.io.inf.busy
  io.done := arithmetic.io.inf.done
  arithmetic.io.inf.resultReady := io.resultReady

  for (bank <- 0 until 4) {
    io.aMem(bank).en := arithmetic.io.inf.aMem(bank).en
    io.aMem(bank).addr := arithmetic.io.inf.aMem(bank).addr
    arithmetic.io.inf.aMem(bank).dout := io.aMem(bank).dout

    io.bMem(bank).en := arithmetic.io.inf.bMem(bank).en
    io.bMem(bank).addr := arithmetic.io.inf.bMem(bank).addr
    arithmetic.io.inf.bMem(bank).dout := io.bMem(bank).dout

    io.cMem(bank).we := arithmetic.io.inf.cMem(bank).we
    io.cMem(bank).addr := arithmetic.io.inf.cMem(bank).addr
    io.cMem(bank).din := arithmetic.io.inf.cMem(bank).din
  }

  val e0Scratch =
    Module(new EvalPairSpRam(
      aWordWidth,
      bWordWidth,
      28,
      useMemoryCompiler
    ))
  val e1Store =
    Seq.fill(2)(Module(new EvalPairSpRam(
      aWordWidth,
      bWordWidth,
      196,
      useMemoryCompiler
    )))
  val i0Store =
    Seq.fill(2)(Module(new StripedSpRam(
      16 * 33,
      196,
      132,
      useMemoryCompiler
    )))
  val i1Store =
    Module(new StripedSpRam(
      16 * 27,
      112,
      144,
      useMemoryCompiler
    ))

  private def connect(
      ram: StripedSpRam,
      port: ToomCookSpBufferRWIO
  ): Unit = {
    ram.io.clk := clock
    ram.io.en := port.en
    ram.io.we := port.we
    ram.io.addr := port.addr
    ram.io.din := port.din
    port.dout := ram.io.dout
  }

  private def connectEvalPair(
      ram: EvalPairSpRam,
      port: ToomCookSpBufferRWIO
  ): Unit = {
    ram.io.clk := clock
    ram.io.en := port.en
    ram.io.we := port.we
    ram.io.addr := port.addr
    ram.io.din := port.din
    port.dout := ram.io.dout
  }

  connectEvalPair(e0Scratch, arithmetic.io.e0Scratch)
  connect(i1Store, arithmetic.io.i1Store)
  for (slot <- 0 until 2) {
    connectEvalPair(e1Store(slot), arithmetic.io.e1Store(slot))
    connect(i0Store(slot), arithmetic.io.i0Store(slot))
  }
}

/**
  * Compatibility wrapper with external input/output SRAMs for simulation.
  */
class ToomCook1024WithSram extends Module {
  val io = IO(new ToomCook1024IO)

  val dut = Module(new ToomCook1024(useMemoryCompiler = false))
  val inARam = Seq.fill(4)(Module(new SpRam(16 * 24, 16)))
  val inBRam = Seq.fill(4)(Module(new SpRam(16 * 8, 16)))
  val outRam =
    Seq.fill(2)(Seq.fill(4)(Module(new SpRam(16 * 24, 16))))

  private def clearRam(ram: SpRam): Unit = {
    ram.io.clk := clock
    ram.io.en := false.B
    ram.io.we := false.B
    ram.io.addr := 0.U.asTypeOf(ram.io.addr)
    ram.io.din := 0.U.asTypeOf(ram.io.din)
  }

  (inARam ++ inBRam ++ outRam.flatten).foreach(clearRam)

  dut.io.start := io.start
  io.busy := dut.io.busy
  io.done := dut.io.done

  when(!dut.io.busy && io.a_we) {
    for (bank <- 0 until 4) {
      inARam(bank).io.en := true.B
      inARam(bank).io.we := true.B
      inARam(bank).io.addr := io.a_addr
      inARam(bank).io.din :=
        io.a_din((bank + 1) * 16 * 24 - 1, bank * 16 * 24)
    }
  }
  when(!dut.io.busy && io.b_we) {
    for (bank <- 0 until 4) {
      inBRam(bank).io.en := true.B
      inBRam(bank).io.we := true.B
      inBRam(bank).io.addr := io.b_addr
      inBRam(bank).io.din :=
        io.b_din((bank + 1) * 16 * 8 - 1, bank * 16 * 8)
    }
  }

  for (bank <- 0 until 4) {
    when(dut.io.aMem(bank).en) {
      inARam(bank).io.en := true.B
      inARam(bank).io.addr := dut.io.aMem(bank).addr
    }
    when(dut.io.bMem(bank).en) {
      inBRam(bank).io.en := true.B
      inBRam(bank).io.addr := dut.io.bMem(bank).addr
    }
    for (lane <- 0 until 16) {
      dut.io.aMem(bank).dout(lane) :=
        inARam(bank).io.dout((lane + 1) * 24 - 1, lane * 24)
      dut.io.bMem(bank).dout(lane) :=
        inBRam(bank).io.dout((lane + 1) * 8 - 1, lane * 8)
    }
  }

  val outFull = RegInit(VecInit(Seq.fill(2)(false.B)))
  val outWriteSlot = RegInit(false.B)
  val outReadSlot = RegInit(false.B)
  dut.io.resultReady := !outFull(outWriteSlot)

  for (slot <- 0 until 2; bank <- 0 until 4) {
    when(outWriteSlot === slot.U && dut.io.cMem(bank).we) {
      outRam(slot)(bank).io.en := true.B
      outRam(slot)(bank).io.we := true.B
      outRam(slot)(bank).io.addr := dut.io.cMem(bank).addr
      outRam(slot)(bank).io.din :=
        Cat(dut.io.cMem(bank).din.reverse)
    }
  }

  val resultComplete =
    dut.io.cMem(0).we && !dut.io.cMem(1).we &&
      dut.io.cMem(0).addr === 0.U
  when(resultComplete) {
    assert(!outFull(outWriteSlot), "external output SRAM overflow")
    outFull(outWriteSlot) := true.B
    outWriteSlot := ~outWriteSlot
  }

  val cReadFire = io.c_re && outFull(outReadSlot)
  when(cReadFire) {
    for (slot <- 0 until 2; bank <- 0 until 4) {
      when(outReadSlot === slot.U) {
        outRam(slot)(bank).io.en := true.B
        outRam(slot)(bank).io.addr := io.c_addr
      }
    }
    when(io.c_addr === 15.U) {
      outFull(outReadSlot) := false.B
      outReadSlot := ~outReadSlot
    }
  }

  io.c_dout := Mux(
    RegNext(outReadSlot, false.B),
    Cat(outRam(1).reverse.map(_.io.dout)),
    Cat(outRam(0).reverse.map(_.io.dout))
  )
  io.c_valid := RegNext(cReadFire, false.B)
}
