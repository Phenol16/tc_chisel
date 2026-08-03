package High

import chisel3._
import chisel3.util._
import core._

class Interp16ColsStep(pidx: Int, inW: Int, outW: Int) extends Module {
  private val mk2 = InterpParamTable.params(pidx).mk2

  val io = IO(new Bundle {
    val in = Input(Vec(7 * 16, UInt(inW.W)))
    val pr0 = Input(UInt(mk2.W))
    val pr1 = Input(UInt(mk2.W))
    val pr2 = Input(UInt(mk2.W))
    val out = Output(Vec(16 * 4, UInt(outW.W)))
    val nr0 = Output(UInt(mk2.W))
    val nr1 = Output(UInt(mk2.W))
    val nr2 = Output(UInt(mk2.W))
  })

  val carry0 = Wire(Vec(17, UInt(mk2.W)))
  val carry1 = Wire(Vec(17, UInt(mk2.W)))
  val carry2 = Wire(Vec(17, UInt(mk2.W)))
  carry0(0) := io.pr0
  carry1(0) := io.pr1
  carry2(0) := io.pr2

  for (col <- 0 until 16) {
    val core = Module(new InterpStepCore(pidx, inW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 16 + col)
    core.io.pr0 := carry0(col)
    core.io.pr1 := carry1(col)
    core.io.pr2 := carry2(col)

    io.out(col * 4 + 0) := ParaMath.mask(core.io.c0part, outW)
    io.out(col * 4 + 1) := ParaMath.mask(core.io.c1part, outW)
    io.out(col * 4 + 2) := ParaMath.mask(core.io.c2part, outW)
    io.out(col * 4 + 3) := ParaMath.mask(core.io.c3, outW)
    carry0(col + 1) := core.io.nr0
    carry1(col + 1) := core.io.nr1
    carry2(col + 1) := core.io.nr2
  }

  io.nr0 := carry0(16)
  io.nr1 := carry1(16)
  io.nr2 := carry2(16)
}

class InterpStepCore(pidx: Int, inW: Int) extends Module {
  private val p = InterpParamTable.params(pidx)
  private val mk = p.mk
  private val mk2 = p.mk2
  private val mk3 = p.mk3

  val io = IO(new Bundle {
    val pIn = Input(Vec(7, UInt(inW.W)))
    val pr0 = Input(UInt(mk2.W))
    val pr1 = Input(UInt(mk2.W))
    val pr2 = Input(UInt(mk2.W))
    val c3 = Output(UInt(mk2.W))
    val c0part = Output(UInt(mk2.W))
    val c1part = Output(UInt(mk2.W))
    val c2part = Output(UInt(mk2.W))
    val nr0 = Output(UInt(mk2.W))
    val nr1 = Output(UInt(mk2.W))
    val nr2 = Output(UInt(mk2.W))
  })

  val p0 = ParaMath.mask(io.pIn(0), mk)
  val p1 = ParaMath.mask(io.pIn(1), mk)
  val p2 = ParaMath.mask(io.pIn(2), mk)
  val p3 = ParaMath.mask(io.pIn(3), mk)
  val p4 = ParaMath.mask(io.pIn(4), mk)
  val p5 = ParaMath.mask(io.pIn(5), mk)
  val p6 = ParaMath.mask(io.pIn(6), mk)

  val r5a = ParaMath.mask(p5 - p4, mk)
  val r3a = ParaMath.mask(ParaMath.mask(p3 - p2, mk) >> 1, mk)
  val r4a = ParaMath.mask(p4 - p0, mk)
  val r4b = ParaMath.mask((r4a << 1) + r5a - (p6 << 7), mk)
  val r2a = ParaMath.mask(p2 + r3a, mk)
  val r1a = ParaMath.mask(p1 + p4 - (r2a << 6) - r2a, mk)
  val r2b = ParaMath.mask(r2a - p6 - p0, mk)
  val r1b = ParaMath.mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mk)
  private val inv3Low =
    (p.inv3 & ((BigInt(1) << mk2) - 1)).U(mk2.W)
  private val inv15Low =
    (p.inv15 & ((BigInt(1) << mk3) - 1)).U(mk3.W)
  private val inv9Low =
    (p.inv9 & ((BigInt(1) << mk3) - 1)).U(mk3.W)
  val r4MulIn = ParaMath.mask(
    ParaMath.mask(r4b - (r2b << 3), mk) >> 3,
    mk2
  )
  val r5MulIn = ParaMath.mask((r5a + r1b) >> 1, mk3)
  val r1MulIn = ParaMath.mask(
    ParaMath.mask(r1b + (r3a << 4), mk) >> 1,
    mk3
  )
  val r4c = ParaMath.mask(r4MulIn * inv3Low, mk2)
  val r5b = ParaMath.mask(r5MulIn * inv15Low, mk3)
  val r1c = ParaMath.mask(r1MulIn * inv9Low, mk3)
  val r2c = ParaMath.mask(r2b - r4c, mk2)
  val r3b = ParaMath.mask(0.U - r3a - r1c, mk2)
  val r5c = ParaMath.mask((r1c - r5b) >> 1, mk2)
  val r1d = ParaMath.mask(r1c - r5c, mk2)

  io.c3 := r3b
  io.c0part := ParaMath.mask(p6 + io.pr2, mk2)
  io.c1part := ParaMath.mask(r5c + io.pr1, mk2)
  io.c2part := ParaMath.mask(r4c + io.pr0, mk2)
  io.nr0 := ParaMath.mask(p0, mk2)
  io.nr1 := r1d
  io.nr2 := r2c
}

/**
  * Four-column fixed-mode I0 engine.
  *
  * Core results only need the low 33 bits at the pidx=1 interpolation input.
  * The pidx=1 result and carry are 30 bits, which is also the exact effective
  * input width of the following pidx=2 interpolation.
  */
class Interp4ColsCoreEngine extends Module {
  private val pidx = 1
  private val inW = InterpStorageWidth.CORE_INPUT_W
  private val outW = InterpStorageWidth.I0_W

  val io = IO(new Bundle {
    val inValid = Input(Bool())
    val inPoint = Input(UInt(3.W))
    val inWord = Input(Vec(16, UInt(inW.W)))

    val busy = Output(Bool())
    val outValid = Output(Bool())
    val outChunk = Output(UInt(2.W))
    val out = Output(Vec(16, UInt(outW.W)))
    val done = Output(Bool())
    val nr0 = Output(UInt(outW.W))
    val nr1 = Output(UInt(outW.W))
    val nr2 = Output(UInt(outW.W))
  })

  val inputReg = Reg(Vec(7 * 16, UInt(inW.W)))
  val pendingReg = Reg(Vec(4, Vec(16, UInt(inW.W))))
  val carry0Reg = Reg(UInt(outW.W))
  val carry1Reg = Reg(UInt(outW.W))
  val carry2Reg = Reg(UInt(outW.W))
  val chunkReg = RegInit(0.U(2.W))
  val busyReg = RegInit(false.B)

  val carry0 = Wire(Vec(5, UInt(outW.W)))
  val carry1 = Wire(Vec(5, UInt(outW.W)))
  val carry2 = Wire(Vec(5, UInt(outW.W)))
  carry0(0) := carry0Reg
  carry1(0) := carry1Reg
  carry2(0) := carry2Reg

  for (col <- 0 until 4) {
    val core = Module(new InterpStepCore(pidx, inW))
    for (point <- 0 until 7) {
      val selected = MuxLookup(
        chunkReg,
        inputReg(point * 16 + col)
      )(Seq(
        1.U -> inputReg(point * 16 + 4 + col),
        2.U -> inputReg(point * 16 + 8 + col),
        3.U -> inputReg(point * 16 + 12 + col)
      ))
      core.io.pIn(point) := selected
    }
    core.io.pr0 := carry0(col)
    core.io.pr1 := carry1(col)
    core.io.pr2 := carry2(col)

    io.out(col * 4 + 0) := core.io.c0part
    io.out(col * 4 + 1) := core.io.c1part
    io.out(col * 4 + 2) := core.io.c2part
    io.out(col * 4 + 3) := core.io.c3
    carry0(col + 1) := core.io.nr0
    carry1(col + 1) := core.io.nr1
    carry2(col + 1) := core.io.nr2
  }

  io.busy := busyReg
  io.outValid := busyReg
  io.outChunk := chunkReg
  io.done := busyReg && chunkReg === 3.U
  io.nr0 := carry0(4)
  io.nr1 := carry1(4)
  io.nr2 := carry2(4)

  when(io.inValid) {
    when(busyReg) {
      assert(
        io.inPoint <= 3.U,
        "Core interpolation overlap exceeds four pending points"
      )
      for (point <- 0 until 4; lane <- 0 until 16) {
        when(io.inPoint === point.U) {
          pendingReg(point)(lane) := io.inWord(lane)
        }
      }
    }.otherwise {
      for (point <- 0 until 7; lane <- 0 until 16) {
        when(io.inPoint === point.U) {
          inputReg(point * 16 + lane) := io.inWord(lane)
        }
      }
    }
  }

  when(busyReg) {
    when(chunkReg === 3.U) {
      busyReg := false.B
      for (point <- 0 until 4; lane <- 0 until 16) {
        inputReg(point * 16 + lane) := Mux(
          io.inValid && io.inPoint === point.U,
          io.inWord(lane),
          pendingReg(point)(lane)
        )
      }
    }.otherwise {
      carry0Reg := carry0(4)
      carry1Reg := carry1(4)
      carry2Reg := carry2(4)
      chunkReg := chunkReg + 1.U
    }
  }.elsewhen(io.inValid && io.inPoint === 6.U) {
    carry0Reg := 0.U
    carry1Reg := 0.U
    carry2Reg := 0.U
    chunkReg := 0.U
    busyReg := true.B
  }
}

/**
  * Shared I1/I2 step.  I1 uses pidx=2 widths 30/27/28 and I2 uses pidx=3
  * widths 27/24/25.  No pidx=1 hardware remains in this engine.
  */
class InterpStepCoreInteShared extends Module {
  private val p2 = InterpParamTable.params(2)

  val io = IO(new Bundle {
    val modeI2 = Input(Bool())
    val pIn = Input(Vec(7, UInt(30.W)))
    val pr0 = Input(UInt(27.W))
    val pr1 = Input(UInt(27.W))
    val pr2 = Input(UInt(27.W))
    val c3 = Output(UInt(27.W))
    val c0part = Output(UInt(27.W))
    val c1part = Output(UInt(27.W))
    val c2part = Output(UInt(27.W))
    val nr0 = Output(UInt(27.W))
    val nr1 = Output(UInt(27.W))
    val nr2 = Output(UInt(27.W))
  })

  private def extend(value: UInt, width: Int): UInt = {
    if (value.getWidth >= width) value(width - 1, 0)
    else Cat(0.U((width - value.getWidth).W), value)
  }

  private def modeMask(value: UInt, i1Width: Int, i2Width: Int,
                       resultWidth: Int): UInt = {
    def resize(width: Int): UInt = {
      val low = if (value.getWidth > width) value(width - 1, 0) else value
      extend(low, resultWidth)
    }
    Mux(io.modeI2, resize(i2Width), resize(i1Width))
  }

  private def maskMk(value: UInt): UInt = modeMask(value, 30, 27, 30)
  private def maskMk2(value: UInt): UInt = modeMask(value, 27, 24, 27)
  private def maskMk3(value: UInt): UInt = modeMask(value, 28, 25, 28)

  val p0 = maskMk(io.pIn(0))
  val p1v = maskMk(io.pIn(1))
  val p2v = maskMk(io.pIn(2))
  val p3v = maskMk(io.pIn(3))
  val p4 = maskMk(io.pIn(4))
  val p5 = maskMk(io.pIn(5))
  val p6 = maskMk(io.pIn(6))

  val r5a = maskMk(p5 - p4)
  val r3a = maskMk(maskMk(p3v - p2v) >> 1)
  val r4a = maskMk(p4 - p0)
  val r4b = maskMk((r4a << 1) + r5a - (p6 << 7))
  val r2a = maskMk(p2v + r3a)
  val r1a = maskMk(p1v + p4 - (r2a << 6) - r2a)
  val r2b = maskMk(r2a - p6 - p0)
  val r1b = maskMk(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5))

  // The low bits of the pidx=2 inverses are also the pidx=3 inverses.
  val inv3 =
    (p2.inv3 & ((BigInt(1) << 27) - 1)).U(27.W)
  val inv9 =
    (p2.inv9 & ((BigInt(1) << 28) - 1)).U(28.W)
  val inv15 =
    (p2.inv15 & ((BigInt(1) << 28) - 1)).U(28.W)

  val r4MulIn = maskMk2(maskMk(r4b - (r2b << 3)) >> 3)
  val r5MulIn = maskMk3(maskMk(r5a + r1b) >> 1)
  val r1MulIn = maskMk3(maskMk(r1b + (r3a << 4)) >> 1)
  val r4c = maskMk2(r4MulIn * inv3)
  val r5b = maskMk3(r5MulIn * inv15)
  val r1c = maskMk3(r1MulIn * inv9)
  val r2c = maskMk2(r2b - r4c)
  val r3b = maskMk2(0.U - r3a - r1c)
  val r5c = maskMk2((r1c - r5b) >> 1)
  val r1d = maskMk2(r1c - r5c)

  val pr0 = maskMk2(io.pr0)
  val pr1 = maskMk2(io.pr1)
  val pr2 = maskMk2(io.pr2)
  io.c3 := r3b
  io.c0part := maskMk2(p6 + pr2)
  io.c1part := maskMk2(r5c + pr1)
  io.c2part := maskMk2(r4c + pr0)
  io.nr0 := maskMk2(p0)
  io.nr1 := r1d
  io.nr2 := r2c
}

/**
  * Four-column interpolation engine shared only by the sequential I1 and I2
  * phases.  Four output chunks are still produced in four cycles.
  */
class Interp4ColsInteEngine extends Module {
  private val inW = InterpStorageWidth.I0_W
  private val outW = InterpStorageWidth.I1_W

  val io = IO(new Bundle {
    val inValid = Input(Bool())
    val inPoint = Input(UInt(3.W))
    val inWord = Input(Vec(16, UInt(inW.W)))
    val modeI2 = Input(Bool())
    val pr0 = Input(UInt(outW.W))
    val pr1 = Input(UInt(outW.W))
    val pr2 = Input(UInt(outW.W))

    val busy = Output(Bool())
    val outValid = Output(Bool())
    val outChunk = Output(UInt(2.W))
    val out = Output(Vec(16, UInt(outW.W)))
    val done = Output(Bool())
    val nr0 = Output(UInt(outW.W))
    val nr1 = Output(UInt(outW.W))
    val nr2 = Output(UInt(outW.W))
  })

  val inputReg = Reg(Vec(7 * 16, UInt(inW.W)))
  val pendingReg = Reg(Vec(4, Vec(16, UInt(inW.W))))
  val modeI2Reg = Reg(Bool())
  val carry0Reg = Reg(UInt(outW.W))
  val carry1Reg = Reg(UInt(outW.W))
  val carry2Reg = Reg(UInt(outW.W))
  val chunkReg = RegInit(0.U(2.W))
  val busyReg = RegInit(false.B)

  val carry0 = Wire(Vec(5, UInt(outW.W)))
  val carry1 = Wire(Vec(5, UInt(outW.W)))
  val carry2 = Wire(Vec(5, UInt(outW.W)))
  carry0(0) := carry0Reg
  carry1(0) := carry1Reg
  carry2(0) := carry2Reg

  for (col <- 0 until 4) {
    val core = Module(new InterpStepCoreInteShared)
    core.io.modeI2 := modeI2Reg
    for (point <- 0 until 7) {
      val selected = MuxLookup(
        chunkReg,
        inputReg(point * 16 + col)
      )(Seq(
        1.U -> inputReg(point * 16 + 4 + col),
        2.U -> inputReg(point * 16 + 8 + col),
        3.U -> inputReg(point * 16 + 12 + col)
      ))
      core.io.pIn(point) := selected
    }
    core.io.pr0 := carry0(col)
    core.io.pr1 := carry1(col)
    core.io.pr2 := carry2(col)

    io.out(col * 4 + 0) := core.io.c0part
    io.out(col * 4 + 1) := core.io.c1part
    io.out(col * 4 + 2) := core.io.c2part
    io.out(col * 4 + 3) := core.io.c3
    carry0(col + 1) := core.io.nr0
    carry1(col + 1) := core.io.nr1
    carry2(col + 1) := core.io.nr2
  }

  io.busy := busyReg
  io.outValid := busyReg
  io.outChunk := chunkReg
  io.done := busyReg && chunkReg === 3.U
  io.nr0 := carry0(4)
  io.nr1 := carry1(4)
  io.nr2 := carry2(4)

  when(io.inValid) {
    when(busyReg) {
      assert(
        io.inPoint <= 3.U,
        "Inte interpolation overlap exceeds four pending points"
      )
      for (point <- 0 until 4; lane <- 0 until 16) {
        when(io.inPoint === point.U) {
          pendingReg(point)(lane) := io.inWord(lane)
        }
      }
    }.otherwise {
      for (point <- 0 until 7; lane <- 0 until 16) {
        when(io.inPoint === point.U) {
          inputReg(point * 16 + lane) := io.inWord(lane)
        }
      }
    }
  }

  when(busyReg) {
    when(chunkReg === 3.U) {
      busyReg := false.B
      for (point <- 0 until 4; lane <- 0 until 16) {
        inputReg(point * 16 + lane) := Mux(
          io.inValid && io.inPoint === point.U,
          io.inWord(lane),
          pendingReg(point)(lane)
        )
      }
    }.otherwise {
      carry0Reg := carry0(4)
      carry1Reg := carry1(4)
      carry2Reg := carry2(4)
      chunkReg := chunkReg + 1.U
    }
  }.elsewhen(io.inValid && io.inPoint === 6.U) {
    modeI2Reg := io.modeI2
    carry0Reg := io.pr0
    carry1Reg := io.pr1
    carry2Reg := io.pr2
    chunkReg := 0.U
    busyReg := true.B
  }
}
