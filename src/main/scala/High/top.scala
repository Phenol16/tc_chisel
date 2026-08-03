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

class ToomCook1024CoreIO(e1BasePairWidth: Int) extends Bundle {
  val inf = new ToomCook1024ExternalIO

  // Eval->Core stores the fixed low E1 bits in the base memory.  Rows whose
  // point pair needs more range also use one dense high-bit extension memory.
  val e1Store = Vec(
    2,
    Flipped(new ToomCookSpBufferRWIO(e1BasePairWidth, 8))
  )
  val e1ExtStore = Vec(
    2,
    Flipped(new ToomCookSpBufferRWIO(160, 8))
  )

  // Core->Inte stores the first interpolation result I0.
  val i0Store = Vec(
    2,
    Flipped(new ToomCookSpBufferRWIO(16 * InterpStorageWidth.I0_W, 8))
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
  private val A_E0_W = EvalStorageWidth.A_E0_W
  private val B_E0_W = EvalStorageWidth.B_E0_W
  private val A_E1_W = EvalStorageWidth.A_E1_W
  private val B_E1_W = EvalStorageWidth.B_E1_W
  private val A_E2_W = EvalStorageWidth.A_E2_W
  private val B_E2_W = EvalStorageWidth.B_E2_W
  private val A_E0_WORD_W = 16 * A_E0_W
  private val B_E0_WORD_W = 16 * B_E0_W
  private val E0_PAIR_W = A_E0_WORD_W + B_E0_WORD_W
  private val A_E1_WORD_W = 16 * A_E1_W
  private val B_E1_WORD_W = 16 * B_E1_W
  private val E1_PAIR_W = A_E1_WORD_W + B_E1_WORD_W
  private val A_E1_BASE_W = 27
  private val B_E1_BASE_W = 13
  private val A_E1_BASE_WORD_W = 16 * A_E1_BASE_W
  private val B_E1_BASE_WORD_W = 16 * B_E1_BASE_W
  private val E1_BASE_PAIR_W = A_E1_BASE_WORD_W + B_E1_BASE_WORD_W
  private val E1_EXT_LANE_W =
    (A_E1_W - A_E1_BASE_W) + (B_E1_W - B_E1_BASE_W)
  private val E1_EXT_WORD_W = 16 * E1_EXT_LANE_W
  private val CORE_INTERP_W = InterpStorageWidth.CORE_INPUT_W
  private val I0_W = InterpStorageWidth.I0_W
  private val I1_W = InterpStorageWidth.I1_W
  private val I2_W = InterpStorageWidth.I2_W
  private val I0_WORD_W = 16 * I0_W
  private val I1_WORD_W = 16 * I1_W
  private val I2_WORD_W = 16 * I2_W

  val io = IO(new ToomCook1024CoreIO(E1_BASE_PAIR_W))
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

  clearRamPort(io.i1Store)
  for (slot <- 0 until 2) {
    clearRamPort(io.e1Store(slot))
    clearRamPort(io.e1ExtStore(slot))
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
  // Eval slot: E0/E1.  One selected point is evaluated and written per cycle.
  // ------------------------------------------------------------------------
  val sharedEvalA =
    Seq.fill(16)(Module(new EvalPoint(A_E1_W, A_E1_W)))
  val sharedEvalB =
    Seq.fill(16)(Module(new EvalPoint(B_E1_W, B_E1_W)))

  /*
   * One outer tile contains four input addresses and four physical banks.
   * Raw input widths are kept here.  E0 is stored at its proven 29/13-bit
   * signed bound and sign-extended to the E1 arithmetic widths before E1.
   */
  private val A_RAW_WORD_W = 16 * 24
  private val B_RAW_WORD_W = 16 * 8
  val evalRawA = Reg(Vec(4, Vec(4, UInt(A_RAW_WORD_W.W))))
  val evalRawB = Reg(Vec(4, Vec(4, UInt(B_RAW_WORD_W.W))))
  val evalMid = Reg(Vec(4, UInt(E0_PAIR_W.W)))

  val Seq(eIdle, eLoad, eLoadWait, eEval0, eEval1) = Enum(5)
  val evalState = RegInit(eIdle)

  val evalOutSlot = RegInit(false.B)
  val evalOuter = RegInit(0.U(2.W))
  val evalLoadInner = RegInit(0.U(2.W))
  val evalInner = RegInit(0.U(2.W))
  val evalPt0 = RegInit(0.U(3.W))
  val evalPt1 = RegInit(0.U(3.W))
  val evalE1Addr = RegInit(0.U(8.W))
  val evalE1ExtAddr = RegInit(0.U(8.W))

  private def e1NeedsExtension(pt0: UInt, pt1: UInt): Bool = {
    val pt0Wide = pt0 === 1.U || pt0 === 4.U || pt0 === 5.U
    val pt1Wide = pt1 === 1.U || pt1 === 4.U || pt1 === 5.U
    val pt0Four = pt0 === 2.U || pt0 === 3.U
    val pt1Four = pt1 === 2.U || pt1 === 3.U
    pt0Wide || pt1Wide || (pt0Four && pt1Four)
  }

  val evalReadFire = evalState === eLoad
  val evalReadValid = RegNext(evalReadFire, false.B)
  val evalReadInnerD = RegEnable(evalLoadInner, evalReadFire)
  val evalReadOuterD = RegEnable(evalOuter, evalReadFire)

  when(evalReadFire) {
    for (bank <- 0 until 4) {
      inf.aMem(bank).en := true.B
      inf.aMem(bank).addr := Cat(evalOuter, evalLoadInner)
      inf.bMem(bank).en := true.B
      inf.bMem(bank).addr := Cat(evalOuter, evalLoadInner)
    }
    when(evalLoadInner === 3.U) {
      evalState := eLoadWait
    }.otherwise {
      evalLoadInner := evalLoadInner + 1.U
    }
  }

  val evalPoint =
    Mux(evalState === eEval0, evalPt0, evalPt1)

  for (lane <- 0 until 16; part <- 0 until 4) {
    val midAWord = evalMid(part)(A_E0_WORD_W - 1, 0)
    val midBWord = evalMid(part)(E0_PAIR_W - 1, A_E0_WORD_W)
    val rawAValue =
      evalRawA(evalInner)(part)((lane + 1) * 24 - 1, lane * 24)
    val rawBValue =
      evalRawB(evalInner)(part)((lane + 1) * 8 - 1, lane * 8)
    val midAValue =
      midAWord((lane + 1) * A_E0_W - 1, lane * A_E0_W)
    val midBValue =
      midBWord((lane + 1) * B_E0_W - 1, lane * B_E0_W)

    sharedEvalA(lane).io.r(part) := Mux(
      evalState === eEval0,
      Cat(0.U((A_E1_W - 24).W), rawAValue),
      Cat(Fill(A_E1_W - A_E0_W, midAValue(A_E0_W - 1)), midAValue)
    )
    sharedEvalB(lane).io.r(part) := Mux(
      evalState === eEval0,
      Cat(0.U((B_E1_W - 8).W), rawBValue),
      Cat(Fill(B_E1_W - B_E0_W, midBValue(B_E0_W - 1)), midBValue)
    )
    sharedEvalA(lane).io.pt := evalPoint
    sharedEvalB(lane).io.pt := evalPoint
  }

  val evalE0PairOut = Cat(
    pack16((0 until 16).map(lane =>
      sharedEvalB(lane).io.out(B_E0_W - 1, 0)
    )),
    pack16((0 until 16).map(lane =>
      sharedEvalA(lane).io.out(A_E0_W - 1, 0)
    ))
  )
  val evalE1BasePairOut = Cat(
    pack16((0 until 16).map(lane =>
      sharedEvalB(lane).io.out(B_E1_BASE_W - 1, 0)
    )),
    pack16((0 until 16).map(lane =>
      sharedEvalA(lane).io.out(A_E1_BASE_W - 1, 0)
    ))
  )
  val evalE1ExtOut = pack16((0 until 16).map { lane =>
    Cat(
      sharedEvalB(lane).io.out(B_E1_W - 1, B_E1_BASE_W),
      sharedEvalA(lane).io.out(A_E1_W - 1, A_E1_BASE_W)
    )
  })
  require(E1_EXT_WORD_W == 160)
  when(evalReadValid) {
    for (bank <- 0 until 4) {
      evalRawA(evalReadInnerD)(bank) :=
        pack16((0 until 16).map(lane => inf.aMem(bank).dout(lane)))
      evalRawB(evalReadInnerD)(bank) :=
        pack16((0 until 16).map(lane => inf.bMem(bank).dout(lane)))
    }
    when(evalState === eLoadWait && evalReadInnerD === 3.U) {
      evalInner := 0.U
      evalPt0 := 0.U
      evalPt1 := 0.U
      evalState := eEval0
    }
    when(evalReadOuterD === 3.U && evalReadInnerD === 3.U) {
      inputFull := false.B
    }
  }

  when(evalState === eEval0) {
    evalMid(evalInner) := evalE0PairOut
    when(evalInner === 3.U) {
      evalInner := 0.U
      evalPt1 := 0.U
      evalState := eEval1
    }.otherwise {
      evalInner := evalInner + 1.U
    }
  }

  when(evalState === eEval1) {
    for (slot <- 0 until 2) {
      when(evalOutSlot === slot.U) {
        io.e1Store(slot).en := true.B
        io.e1Store(slot).we := true.B
        io.e1Store(slot).addr := evalE1Addr
        io.e1Store(slot).din := evalE1BasePairOut
        when(e1NeedsExtension(evalPt0, evalPt1)) {
          assert(evalE1ExtAddr < 148.U, "E1 extension write address overflow")
          io.e1ExtStore(slot).en := true.B
          io.e1ExtStore(slot).we := true.B
          io.e1ExtStore(slot).addr := evalE1ExtAddr
          io.e1ExtStore(slot).din := evalE1ExtOut
        }
      }
    }
    when(e1NeedsExtension(evalPt0, evalPt1)) {
      evalE1ExtAddr := evalE1ExtAddr + 4.U
    }
    when(evalPt1 === 6.U) {
      evalPt1 := 0.U
      when(evalPt0 =/= 6.U) {
        evalE1Addr := evalE1Addr - 164.U
        evalPt0 := evalPt0 + 1.U
        evalInner := 0.U
        evalState := eEval0
      }.otherwise {
        when(evalOuter === 3.U) {
          e1State(evalOutSlot) := memReady
          evalState := eIdle
        }.otherwise {
          evalE1Addr := evalOuter + 1.U
          evalE1ExtAddr := evalOuter + 1.U
          evalOuter := evalOuter + 1.U
          evalLoadInner := 0.U
          evalPt0 := 0.U
          evalState := eLoad
        }
      }
    }.otherwise {
      evalE1Addr := evalE1Addr + 28.U
      evalPt1 := evalPt1 + 1.U
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
    evalLoadInner := 0.U
    evalInner := 0.U
    evalPt0 := 0.U
    evalPt1 := 0.U
    evalE1Addr := 0.U
    evalE1ExtAddr := 0.U
    evalState := eLoad
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

  val coreInput = Reg(Vec(2, Vec(4, UInt(E1_PAIR_W.W))))
  val coreCurSel = RegInit(false.B)

  val pointEvalA =
    Seq.fill(16)(Module(new EvalPoint(A_E2_W, A_E2_W)))
  val pointEvalB =
    Seq.fill(16)(Module(new EvalPoint(B_E2_W, B_E2_W)))

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
  val coreE1Addr = RegInit(0.U(8.W))
  val coreE1ExtAddr = RegInit(0.U(8.W))

  val corePrimeRead = coreState === cPrime
  val corePrefetchRead =
    coreState === cRun && corePt2 >= 1.U && corePt2 <= 4.U &&
      (coreGroup =/= 48.U || coreChainValid)
  val coreE1ReadFire = corePrimeRead || corePrefetchRead

  val coreReadPart = Wire(UInt(2.W))
  val coreReadSlot = Wire(Bool())
  val coreReadCaptureSel = Wire(Bool())
  val coreReadNeedsExt = Wire(Bool())
  val coreReadPt1 = Wire(UInt(3.W))

  coreReadPart := corePrimePart
  coreReadSlot := coreInSlot
  coreReadCaptureSel := false.B
  coreReadNeedsExt := false.B
  coreReadPt1 := 0.U

  when(corePrefetchRead) {
    coreReadPart := corePt2 - 1.U
    coreReadCaptureSel := !coreCurSel
    when(coreGroup === 48.U) {
      coreReadSlot := coreChainInSlot
      coreReadNeedsExt := false.B
      coreReadPt1 := 0.U
    }.otherwise {
      val nextPt0 =
        Mux(corePt1 === 6.U, corePt0 + 1.U, corePt0)
      val nextPt1 =
        Mux(corePt1 === 6.U, 0.U, corePt1 + 1.U)
      coreReadSlot := coreInSlot
      coreReadNeedsExt := e1NeedsExtension(nextPt0, nextPt1)
      coreReadPt1 := nextPt1
    }
  }

  when(coreE1ReadFire) {
    for (slot <- 0 until 2) {
      when(coreReadSlot === slot.U) {
        io.e1Store(slot).en := true.B
        io.e1Store(slot).addr := coreE1Addr
        when(coreReadNeedsExt) {
          assert(coreE1ExtAddr < 148.U, "E1 extension read address overflow")
          io.e1ExtStore(slot).en := true.B
          io.e1ExtStore(slot).addr := coreE1ExtAddr
        }
      }
    }
    when(coreReadPart === 3.U) {
      when(coreReadPt1 === 6.U) {
        // End of a pt1 sweep:
        //   (pt1=6, outer=3) -> (next pt0, pt1=0, outer=0)
        coreE1Addr := coreE1Addr - 167.U
      }.otherwise {
        // The next logical group advances pt1.  Physical E1 rows are
        // transposed, so its outer=0 address is 25 after the current outer=3.
        coreE1Addr := coreE1Addr + 25.U
      }
    }.otherwise {
      coreE1Addr := coreE1Addr + 1.U
    }
    when(coreReadNeedsExt) {
      coreE1ExtAddr := coreE1ExtAddr + 1.U
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
  val coreReadNeedsExtD =
    RegEnable(coreReadNeedsExt, coreE1ReadFire)

  val coreReadBaseWord = Mux(
    coreReadSlotD,
    io.e1Store(1).dout,
    io.e1Store(0).dout
  )
  val coreReadExtWord = Mux(
    coreReadSlotD,
    io.e1ExtStore(1).dout,
    io.e1ExtStore(0).dout
  )
  val coreReadBaseA =
    coreReadBaseWord(A_E1_BASE_WORD_W - 1, 0)
  val coreReadBaseB =
    coreReadBaseWord(E1_BASE_PAIR_W - 1, A_E1_BASE_WORD_W)
  val coreReadA = Wire(Vec(16, UInt(A_E1_W.W)))
  val coreReadB = Wire(Vec(16, UInt(B_E1_W.W)))
  for (lane <- 0 until 16) {
    val baseA =
      coreReadBaseA((lane + 1) * A_E1_BASE_W - 1, lane * A_E1_BASE_W)
    val baseB =
      coreReadBaseB((lane + 1) * B_E1_BASE_W - 1, lane * B_E1_BASE_W)
    val ext =
      coreReadExtWord((lane + 1) * E1_EXT_LANE_W - 1, lane * E1_EXT_LANE_W)
    coreReadA(lane) := Mux(
      coreReadNeedsExtD,
      Cat(ext(A_E1_W - A_E1_BASE_W - 1, 0), baseA),
      Cat(Fill(A_E1_W - A_E1_BASE_W, baseA(A_E1_BASE_W - 1)), baseA)
    )
    coreReadB(lane) := Mux(
      coreReadNeedsExtD,
      Cat(
        ext(E1_EXT_LANE_W - 1, A_E1_W - A_E1_BASE_W),
        baseB
      ),
      Cat(Fill(B_E1_W - B_E1_BASE_W, baseB(B_E1_BASE_W - 1)), baseB)
    )
  }
  val coreReadWord = Cat(pack16(coreReadB), pack16(coreReadA))

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
      val aWord = pair(A_E1_WORD_W - 1, 0)
      val bWord = pair(E1_PAIR_W - 1, A_E1_WORD_W)
      val aStored =
        aWord((lane + 1) * A_E1_W - 1, lane * A_E1_W)
      val bStored =
        bWord((lane + 1) * B_E1_W - 1, lane * B_E1_W)
      pointEvalA(lane).io.r(part) :=
        Cat(Fill(A_E2_W - A_E1_W, aStored(A_E1_W - 1)), aStored)
      pointEvalB(lane).io.r(part) :=
        Cat(Fill(B_E2_W - B_E1_W, bStored(B_E1_W - 1)), bStored)
    }
    pointEvalA(lane).io.pt := corePt2
    pointEvalB(lane).io.pt := corePt2
    core.io.a(lane) := Cat(
      Fill(A_EVAL_W - A_E2_W, pointEvalA(lane).io.out(A_E2_W - 1)),
      pointEvalA(lane).io.out
    )
    core.io.b(lane) := Cat(
      Fill(B_EVAL_W - B_E2_W, pointEvalB(lane).io.out(B_E2_W - 1)),
      pointEvalB(lane).io.out
    )
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
    coreE1Addr := 0.U
    coreE1ExtAddr := 0.U
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
    coreE1Addr := 0.U
    coreE1ExtAddr := 0.U
    coreState := cPrime
  }

  val coreMetaValid =
    ShiftRegister(coreInputValid, 4, false.B, true.B)
  val coreMetaGroup = ShiftRegister(coreGroup, 4)
  val coreMetaPoint = ShiftRegister(corePt2, 4)
  val coreMetaSlot = ShiftRegister(coreOutSlot, 4)
  val coreInterpWord =
    pack16((0 until 16).map(index =>
      core.io.c(index)(CORE_INTERP_W - 1, 0)
    ))

  when(core.io.valid_out || coreMetaValid) {
    assert(
      core.io.valid_out === coreMetaValid,
      "Core latency changed: update High metadata delay"
    )
  }

  val interpCore = Module(new Interp4ColsCoreEngine)
  val coreInterpStart =
    core.io.valid_out && coreMetaValid && coreMetaPoint === 6.U
  interpCore.io.inValid := core.io.valid_out && coreMetaValid
  interpCore.io.inPoint := coreMetaPoint
  for (lane <- 0 until 16) {
    interpCore.io.inWord(lane) :=
      coreInterpWord((lane + 1) * CORE_INTERP_W - 1, lane * CORE_INTERP_W)
  }

  val i0EngineGroup = Reg(UInt(6.W))
  val i0EngineSlot = Reg(Bool())
  val i0FirstWord = Reg(UInt(I0_WORD_W.W))
  val i0PatchWord = Reg(UInt(I0_WORD_W.W))
  val i0PatchValid = RegInit(false.B)

  when(coreInterpStart) {
    assert(!interpCore.io.busy, "I0 interpolation engine overlap")
    i0EngineGroup := coreMetaGroup
    i0EngineSlot := coreMetaSlot
  }

  val i0EngineWord = pack16((0 until 16).map { index =>
    interpCore.io.out(index)
  })

  when(interpCore.io.outValid) {
    when(interpCore.io.outChunk === 0.U) {
      i0FirstWord := i0EngineWord
    }.otherwise {
      for (slot <- 0 until 2) {
        when(i0EngineSlot === slot.U) {
          io.i0Store(slot).en := true.B
          io.i0Store(slot).we := true.B
          io.i0Store(slot).addr :=
            Cat(i0EngineGroup, interpCore.io.outChunk)
          io.i0Store(slot).din := i0EngineWord
        }
      }
    }

    when(interpCore.io.done) {
      val first = split16(i0FirstWord, I0_W)
      val corrected = Wire(Vec(16, UInt(I0_W.W)))
      corrected := first
      corrected(0) :=
        ParaMath.mask(first(0) - interpCore.io.nr2, I0_W)
      corrected(1) :=
        ParaMath.mask(first(1) - interpCore.io.nr1, I0_W)
      corrected(2) :=
        ParaMath.mask(first(2) - interpCore.io.nr0, I0_W)
      i0PatchWord := pack16(corrected)
      i0PatchValid := true.B
    }
  }

  when(i0PatchValid) {
    for (slot <- 0 until 2) {
      when(i0EngineSlot === slot.U) {
        io.i0Store(slot).en := true.B
        io.i0Store(slot).we := true.B
        io.i0Store(slot).addr := Cat(i0EngineGroup, 0.U(2.W))
        io.i0Store(slot).din := i0PatchWord
      }
    }
    i0PatchValid := false.B
    when(i0EngineGroup === 48.U) {
      i0State(i0EngineSlot) := memReady
    }
  }

  // ------------------------------------------------------------------------
  // Inte slot: sequentially gather seven point words, then execute I1/I2.
  // ------------------------------------------------------------------------
  val interpInte = Module(new Interp4ColsInteEngine)
  interpInte.io.inValid := false.B
  interpInte.io.inPoint := 0.U
  interpInte.io.inWord := VecInit(Seq.fill(16)(0.U(I0_W.W)))
  interpInte.io.modeI2 := false.B
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
  val i1ReadAddr = RegInit(0.U(8.W))
  val i1WriteAddr = RegInit(0.U(7.W))
  val inteCarry0 = RegInit(0.U(I1_W.W))
  val inteCarry1 = RegInit(0.U(I1_W.W))
  val inteCarry2 = RegInit(0.U(I1_W.W))
  val inteFirstWord = Reg(UInt(I1_WORD_W.W))
  val i1EngineP0 = Reg(UInt(3.W))
  val i1EngineChunk = Reg(UInt(2.W))
  val intePatchWord = Reg(UInt(I1_WORD_W.W))
  val i1PatchValid = RegInit(false.B)
  val inteEngineIsI2 = RegInit(false.B)

  val i1ReadFire = inteState === iI1 && !i1IssueDone
  val i1ReadValid = RegNext(i1ReadFire, false.B)
  val i1ReadP0D = RegEnable(i1IssueP0, i1ReadFire)
  val i1ReadChunkD = RegEnable(i1IssueChunk, i1ReadFire)
  val i1ReadPointD = RegEnable(i1IssuePoint, i1ReadFire)

  when(i1ReadFire) {
    for (slot <- 0 until 2) {
      when(inteInSlot === slot.U) {
        io.i0Store(slot).en := true.B
        io.i0Store(slot).addr := i1ReadAddr
      }
    }

    when(i1IssuePoint === 6.U) {
      i1IssuePoint := 0.U
      when(i1IssueChunk === 3.U) {
        i1ReadAddr := i1ReadAddr + 1.U
        i1IssueChunk := 0.U
        when(i1IssueP0 === 6.U) {
          i1IssueDone := true.B
        }.otherwise {
          i1IssueP0 := i1IssueP0 + 1.U
        }
      }.otherwise {
        i1ReadAddr := i1ReadAddr - 23.U
        i1IssueChunk := i1IssueChunk + 1.U
      }
    }.otherwise {
      i1ReadAddr := i1ReadAddr + 4.U
      i1IssuePoint := i1IssuePoint + 1.U
    }
  }

  val i1ReadWord = Mux(
    inteInSlot,
    io.i0Store(1).dout,
    io.i0Store(0).dout
  )

  when(inteState === iI1) {
    interpInte.io.modeI2 := false.B
    interpInte.io.pr0 :=
      Mux(i1ReadChunkD === 0.U, 0.U, inteCarry0)
    interpInte.io.pr1 :=
      Mux(i1ReadChunkD === 0.U, 0.U, inteCarry1)
    interpInte.io.pr2 :=
      Mux(i1ReadChunkD === 0.U, 0.U, inteCarry2)
    when(i1ReadValid) {
      interpInte.io.inValid := true.B
      interpInte.io.inPoint := i1ReadPointD
      for (lane <- 0 until 16) {
        interpInte.io.inWord(lane) :=
          i1ReadWord((lane + 1) * I0_W - 1, lane * I0_W)
      }
    }
  }

  when(i1ReadValid && i1ReadPointD === 6.U) {
    assert(!interpInte.io.busy, "I1 interpolation engine overlap")
    inteEngineIsI2 := false.B
    i1EngineP0 := i1ReadP0D
    i1EngineChunk := i1ReadChunkD
  }

  val i1EngineWord = pack16((0 until 16).map { index =>
    interpInte.io.out(index)(26, 0)
  })

  when(interpInte.io.outValid && !inteEngineIsI2) {
    io.i1Store.en := true.B
    io.i1Store.we := true.B
    io.i1Store.addr := i1WriteAddr
    io.i1Store.din := i1EngineWord
    i1WriteAddr := i1WriteAddr + 1.U

    when(i1EngineChunk === 0.U && interpInte.io.outChunk === 0.U) {
      inteFirstWord := i1EngineWord
    }

    when(interpInte.io.done) {
      inteCarry0 := interpInte.io.nr0
      inteCarry1 := interpInte.io.nr1
      inteCarry2 := interpInte.io.nr2

      when(i1EngineChunk === 3.U) {
        val first = split16(inteFirstWord, I1_W)
        val corrected = Wire(Vec(16, UInt(I1_W.W)))
        corrected := first
        corrected(0) :=
          ParaMath.mask(first(0) - interpInte.io.nr2, I1_W)
        corrected(1) :=
          ParaMath.mask(first(1) - interpInte.io.nr1, I1_W)
        corrected(2) :=
          ParaMath.mask(first(2) - interpInte.io.nr0, I1_W)
        intePatchWord := pack16(corrected)
        i1PatchValid := true.B
        when(i1EngineP0 === 6.U) {
          i0State(inteInSlot) := memEmpty
        }
      }
    }
  }

  when(i1PatchValid) {
    assert(
      !(interpInte.io.outValid && !inteEngineIsI2),
      "I1 SRAM patch conflicts with interpolation output"
    )
    io.i1Store.en := true.B
    io.i1Store.we := true.B
    io.i1Store.addr := Cat(i1EngineP0, 0.U(4.W))
    io.i1Store.din := intePatchWord
    i1PatchValid := false.B
    when(i1EngineP0 === 6.U) {
      inteCarry0 := 0.U
      inteCarry1 := 0.U
      inteCarry2 := 0.U
      inteState := iI2
    }
  }

  val i2IssueChunk = RegInit(0.U(4.W))
  val i2IssuePoint = RegInit(0.U(3.W))
  val i2IssueDone = RegInit(false.B)
  val i2EngineChunk = Reg(UInt(4.W))
  val i2ReadAddr = RegInit(0.U(7.W))

  val i2ReadFire = inteState === iI2 && !i2IssueDone
  val i2ReadValid = RegNext(i2ReadFire, false.B)
  val i2ReadChunkD = RegEnable(i2IssueChunk, i2ReadFire)
  val i2ReadPointD = RegEnable(i2IssuePoint, i2ReadFire)

  when(i2ReadFire) {
    io.i1Store.en := true.B
    io.i1Store.addr := i2ReadAddr
    when(i2IssuePoint === 6.U) {
      i2IssuePoint := 0.U
      when(i2IssueChunk === 15.U) {
        i2IssueDone := true.B
      }.otherwise {
        i2ReadAddr := i2ReadAddr - 95.U
        i2IssueChunk := i2IssueChunk + 1.U
      }
    }.otherwise {
      i2ReadAddr := i2ReadAddr + 16.U
      i2IssuePoint := i2IssuePoint + 1.U
    }
  }

  when(inteState === iI2 || inteState === iI2Patch) {
    interpInte.io.modeI2 := true.B
    interpInte.io.pr0 := inteCarry0
    interpInte.io.pr1 := inteCarry1
    interpInte.io.pr2 := inteCarry2
    when(i2ReadValid) {
      interpInte.io.inValid := true.B
      interpInte.io.inPoint := i2ReadPointD
      for (lane <- 0 until 16) {
        val value =
          io.i1Store.dout((lane + 1) * I1_W - 1, lane * I1_W)
        interpInte.io.inWord(lane) :=
          Cat(0.U((I0_W - I1_W).W), value)
      }
    }
  }

  when(i2ReadValid && i2ReadPointD === 6.U) {
    assert(!interpInte.io.busy, "I2 interpolation engine overlap")
    inteEngineIsI2 := true.B
    i2EngineChunk := i2ReadChunkD
  }

  val i2EngineWord = pack16((0 until 16).map { index =>
    interpInte.io.out(index)(23, 0)
  })

  when(interpInte.io.outValid && inteEngineIsI2) {
    for (bank <- 0 until 4) {
      when(interpInte.io.outChunk === bank.U) {
        inf.cMem(bank).we := true.B
        inf.cMem(bank).addr := i2EngineChunk
        inf.cMem(bank).din := split16(i2EngineWord, 24)
      }
    }

    when(i2EngineChunk === 0.U && interpInte.io.outChunk === 0.U) {
      inteFirstWord :=
        Cat(0.U((I1_WORD_W - I2_WORD_W).W), i2EngineWord)
    }

    when(interpInte.io.done) {
      inteCarry0 := Cat(0.U((I1_W - I2_W).W), interpInte.io.nr0(23, 0))
      inteCarry1 := Cat(0.U((I1_W - I2_W).W), interpInte.io.nr1(23, 0))
      inteCarry2 := Cat(0.U((I1_W - I2_W).W), interpInte.io.nr2(23, 0))

      when(i2EngineChunk === 15.U) {
        val first = split16(inteFirstWord(I2_WORD_W - 1, 0), I2_W)
        val corrected = Wire(Vec(16, UInt(I2_W.W)))
        corrected := first
        corrected(0) :=
          ParaMath.mask(first(0) - interpInte.io.nr2(23, 0), I2_W)
        corrected(1) :=
          ParaMath.mask(first(1) - interpInte.io.nr1(23, 0), I2_W)
        corrected(2) :=
          ParaMath.mask(first(2) - interpInte.io.nr0(23, 0), I2_W)
        intePatchWord :=
          Cat(0.U((I1_WORD_W - I2_WORD_W).W), pack16(corrected))
        inteState := iI2Patch
      }
    }
  }

  when(inteState === iI2Patch) {
    inf.cMem(0).we := true.B
    inf.cMem(0).addr := 0.U
    inf.cMem(0).din :=
      split16(intePatchWord(I2_WORD_W - 1, 0), I2_W)
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
    i1ReadAddr := 0.U
    i1WriteAddr := 0.U
    inteCarry0 := 0.U
    inteCarry1 := 0.U
    inteCarry2 := 0.U
    i1PatchValid := false.B

    i2IssueChunk := 0.U
    i2IssuePoint := 0.U
    i2IssueDone := false.B
    i2ReadAddr := 0.U
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

  private val e1BasePairWidth = 16 * (27 + 13)
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

  val e1Store =
    Seq.fill(2)(Module(new StripedSpRam(
      e1BasePairWidth,
      196,
      160,
      useMemoryCompiler
    )))
  val e1ExtStore =
    Seq.fill(2)(Module(new StripedSpRam(
      160,
      148,
      160,
      useMemoryCompiler
    )))
  val i0Store =
    Seq.fill(2)(Module(new StripedSpRam(
      16 * InterpStorageWidth.I0_W,
      196,
      160,
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

  connect(i1Store, arithmetic.io.i1Store)
  for (slot <- 0 until 2) {
    connect(e1Store(slot), arithmetic.io.e1Store(slot))
    connect(e1ExtStore(slot), arithmetic.io.e1ExtStore(slot))
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

  /*
   * The four-column interpolation engine writes bank 0/address 0 once for the
   * first raw chunk and once more for the final negacyclic correction.  Only
   * the latter completes a result.  Bank 3/address 15 is the final raw word,
   * so it arms the following bank 0/address 0 patch as the completion event.
   */
  val waitFinalPatch = RegInit(false.B)
  when(
    dut.io.cMem(3).we &&
      dut.io.cMem(3).addr === 15.U
  ) {
    waitFinalPatch := true.B
  }
  val resultComplete =
    waitFinalPatch &&
      dut.io.cMem(0).we && !dut.io.cMem(1).we &&
      !dut.io.cMem(2).we && !dut.io.cMem(3).we &&
      dut.io.cMem(0).addr === 0.U
  when(resultComplete) {
    assert(!outFull(outWriteSlot), "external output SRAM overflow")
    outFull(outWriteSlot) := true.B
    outWriteSlot := ~outWriteSlot
    waitFinalPatch := false.B
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
