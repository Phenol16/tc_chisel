package core
import chisel3._
import chisel3.util._
class Interpolation(
    stride: Int,
    inWidth: Int,
    outWidth: Int,
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val w = Input(Vec(7 * stride, UInt(inWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4 * stride, UInt(outWidth.W)))
  })

  val interp = Module(new InterpolationComb(stride, inWidth, outWidth))
  interp.io.w := io.w

  val validReg = RegNext(io.valid_in, false.B)
  val cReg = Reg(Vec(4 * stride, UInt(outWidth.W)))
  cReg := interp.io.c

  io.valid_out := validReg
  io.c := cReg
}

class InterpolationComb(
    stride: Int,
    inWidth: Int,
    outWidth: Int,
) extends Module {
  val io = IO(new Bundle {
    val w = Input(Vec(7 * stride, UInt(inWidth.W)))
    val c = Output(Vec(4 * stride, UInt(outWidth.W)))
  })


private val inv3  = MagicNumber.inv3(outWidth).U(outWidth.W)
private val inv9  = MagicNumber.inv9(inWidth - 2).U((inWidth - 2).W)
private val inv15 = MagicNumber.inv15(inWidth - 2).U((inWidth - 2).W)
  val cRaw = Wire(Vec(4 * stride, UInt(outWidth.W)))
  val prevR0 = Wire(Vec(stride + 1, UInt(outWidth.W)))
  val prevR1 = Wire(Vec(stride + 1, UInt(outWidth.W)))
  val prevR2 = Wire(Vec(stride + 1, UInt(outWidth.W)))

  prevR0(0) := 0.U
  prevR1(0) := 0.U
  prevR2(0) := 0.U

  for (i <- 0 until stride) {
    val p0 = ParaMath.mask(io.w(i), inWidth)
    val p1 = ParaMath.mask(io.w(stride + i), inWidth)
    val p2 = ParaMath.mask(io.w(2 * stride + i), inWidth)
    val p3 = ParaMath.mask(io.w(3 * stride + i), inWidth)
    val p4 = ParaMath.mask(io.w(4 * stride + i), inWidth)
    val p5 = ParaMath.mask(io.w(5 * stride + i), inWidth)
    val p6 = ParaMath.mask(io.w(6 * stride + i), inWidth)

    val r1a = ParaMath.mask(p1 + p4, inWidth)
    val r5a = ParaMath.mask(p5 - p4, inWidth)
    val r3a = ParaMath.mask(ParaMath.mask(p3 - p2, inWidth) >> 1, inWidth)
    val r4a = ParaMath.mask(p4 - p0, inWidth)
    val r4b = ParaMath.mask((r4a << 1) + r5a - (p6 << 7), inWidth)
    val r2a = ParaMath.mask(p2 + r3a, inWidth)
    val r1b = ParaMath.mask(r1a - (r2a << 6) - r2a, inWidth)
    val r2b = ParaMath.mask(r2a - p6 - p0, inWidth)
    val r1c = ParaMath.mask(r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), inWidth)

    val r4d = ParaMath.mask(
      ParaMath.mask(ParaMath.mask(r4b - (r2b << 3), inWidth) >> 3, inWidth) * inv3,
      outWidth
    )
    val r5c = ParaMath.mask(
      ParaMath.mask((r5a + r1c) >> 1, inWidth) * inv15,
      inWidth-2
    )
    val r1e = ParaMath.mask(
      ParaMath.mask(ParaMath.mask(r1c + (r3a << 4), inWidth) >> 1, inWidth) * inv9,
      inWidth-2
    )

    val r2c = ParaMath.mask(r2b - r4d, outWidth)
    val r3b = ParaMath.mask(0.U - r1e - r3a, outWidth)
    val r5d = ParaMath.mask((r1e - r5c) >> 1, outWidth)
    val r1f = ParaMath.mask(r1e - r5d, outWidth)

    cRaw(4 * i + 0) := ParaMath.mask(p6 + prevR2(i), outWidth)
    cRaw(4 * i + 1) := ParaMath.mask(r5d + prevR1(i), outWidth)
    cRaw(4 * i + 2) := ParaMath.mask(r4d + prevR0(i), outWidth)
    cRaw(4 * i + 3) := ParaMath.mask(r3b, outWidth)

    prevR0(i + 1) := ParaMath.mask(p0, outWidth)
    prevR1(i + 1) := r1f
    prevR2(i + 1) := r2c
  }

  for (i <- 0 until 4 * stride) {
    io.c(i) := cRaw(i)
  }
  io.c(0) := ParaMath.mask(cRaw(0) - prevR2(stride), outWidth)
  io.c(1) := ParaMath.mask(cRaw(1) - prevR1(stride), outWidth)
  io.c(2) := ParaMath.mask(cRaw(2) - prevR0(stride), outWidth)
}