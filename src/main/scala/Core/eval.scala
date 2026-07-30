package core
import chisel3._
import chisel3.util._
class Eval(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(4, UInt(inWidth.W)))
    val out = Output(Vec(7, UInt(outWidth.W)))
  })

  val even = io.in(0) +& io.in(2)
  val odd = io.in(1) +& io.in(3)
  val scaledEven = Cat(io.in(0), 0.U(2.W)) +& io.in(2)
  val scaledOdd = Cat(io.in(1), 0.U(2.W)) +& io.in(3)

  val high0 = io.in(2) +& Cat(io.in(3), 0.U(1.W))
  val high1 = io.in(1) +& Cat(high0, 0.U(1.W))
  val high2 = io.in(0) +& Cat(high1, 0.U(1.W))

  io.out(0) := io.in(3)
  io.out(1) := high2
  io.out(2) := even +& odd
  io.out(3) := ParaMath.fillMsb(even -& odd, outWidth)
  io.out(4) := Cat(scaledEven, 0.U(1.W)) +& scaledOdd
  io.out(5) := ParaMath.fillMsb(Cat(scaledEven, 0.U(1.W)) -& scaledOdd, outWidth)
  io.out(6) := io.in(0)
}