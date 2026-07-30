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
  val r4c = ParaMath.mask(ParaMath.mask(ParaMath.mask(r4b - (r2b << 3), mk) >> 3, mk) * p.inv3.U(42.W), mk2)
  val r5b = ParaMath.mask(ParaMath.mask((r5a + r1b) >> 1, mk) * p.inv15.U(42.W), mk3)
  val r1c = ParaMath.mask(ParaMath.mask(ParaMath.mask(r1b + (r3a << 4), mk) >> 1, mk) * p.inv9.U(42.W), mk3)
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
  * Runtime-selectable outer interpolation datapath.
  *
  * mode 0/1/2 implements pidx 1/2/3 respectively.  Fixed maximum-width ports
  * let the three strictly sequential High-level interpolation phases share one
  * physical 16-column array.
  */
class Interp16ColsShared extends Module {
  val io = IO(new Bundle {
    val mode = Input(UInt(2.W))
    val in = Input(Vec(7 * 16, UInt(36.W)))
    val pr0 = Input(UInt(30.W))
    val pr1 = Input(UInt(30.W))
    val pr2 = Input(UInt(30.W))
    val out = Output(Vec(16 * 4, UInt(33.W)))
    val nr0 = Output(UInt(30.W))
    val nr1 = Output(UInt(30.W))
    val nr2 = Output(UInt(30.W))
  })

  val carry0 = Wire(Vec(17, UInt(30.W)))
  val carry1 = Wire(Vec(17, UInt(30.W)))
  val carry2 = Wire(Vec(17, UInt(30.W)))
  carry0(0) := io.pr0
  carry1(0) := io.pr1
  carry2(0) := io.pr2

  for (col <- 0 until 16) {
    val core = Module(new InterpStepCoreShared)
    core.io.mode := io.mode
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 16 + col)
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

  io.nr0 := carry0(16)
  io.nr1 := carry1(16)
  io.nr2 := carry2(16)
}

class InterpStepCoreShared extends Module {
  private val p1 = InterpParamTable.params(1)

  val io = IO(new Bundle {
    val mode = Input(UInt(2.W))
    val pIn = Input(Vec(7, UInt(36.W)))
    val pr0 = Input(UInt(30.W))
    val pr1 = Input(UInt(30.W))
    val pr2 = Input(UInt(30.W))
    val c3 = Output(UInt(33.W))
    val c0part = Output(UInt(33.W))
    val c1part = Output(UInt(33.W))
    val c2part = Output(UInt(33.W))
    val nr0 = Output(UInt(30.W))
    val nr1 = Output(UInt(30.W))
    val nr2 = Output(UInt(30.W))
  })

  private def extend(value: UInt, width: Int): UInt = {
    if (value.getWidth >= width) value(width - 1, 0)
    else Cat(0.U((width - value.getWidth).W), value)
  }

  private def modeMask(
      value: UInt,
      width0: Int,
      width1: Int,
      width2: Int,
      resultWidth: Int
  ): UInt = {
    def resize(width: Int): UInt = {
      val low = if (value.getWidth > width) value(width - 1, 0) else value
      extend(low, resultWidth)
    }
    MuxLookup(io.mode, resize(width0))(Seq(
      0.U -> resize(width0),
      1.U -> resize(width1),
      2.U -> resize(width2)
    ))
  }

  private def maskMk(value: UInt): UInt = modeMask(value, 33, 30, 27, 33)
  private def maskMk2(value: UInt): UInt = modeMask(value, 30, 27, 24, 30)
  private def maskMk3(value: UInt): UInt = modeMask(value, 31, 28, 25, 31)

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

  // For odd d, inv(d) modulo 2^n is the low n bits of inv(d) modulo 2^N.
  // Therefore the widest constants also implement modes 1/2 after maskMk2/3
  // truncation.  Keeping these operands constant avoids general multipliers.
  val inv3 = p1.inv3.U(42.W)
  val inv9 = p1.inv9.U(42.W)
  val inv15 = p1.inv15.U(42.W)

  val r4c = maskMk2((maskMk(r4b - (r2b << 3)) >> 3) * inv3)
  val r5b = maskMk3((maskMk(r5a + r1b) >> 1) * inv15)
  val r1c = maskMk3((maskMk(r1b + (r3a << 4)) >> 1) * inv9)
  val r2c = maskMk2(r2b - r4c)
  val r3b = maskMk2(0.U - r3a - r1c)
  val r5c = maskMk2((r1c - r5b) >> 1)
  val r1d = maskMk2(r1c - r5c)

  val pr0 = maskMk2(io.pr0)
  val pr1 = maskMk2(io.pr1)
  val pr2 = maskMk2(io.pr2)
  io.c3 := extend(r3b, 33)
  io.c0part := extend(maskMk2(p6 + pr2), 33)
  io.c1part := extend(maskMk2(r5c + pr1), 33)
  io.c2part := extend(maskMk2(r4c + pr0), 33)
  io.nr0 := maskMk2(p0)
  io.nr1 := r1d
  io.nr2 := r2c
}
