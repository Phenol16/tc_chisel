package High

import chisel3._
import chisel3.util._
import core._

class Eval64Point(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(64, UInt(inW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val mid = Wire(Vec(16, UInt(outW.W)))
  for (outer <- 0 until 4) {
    for (middle <- 0 until 4) {
      val eval = Module(new EvalPoint(inW, outW))
      for (inner <- 0 until 4) eval.io.r(inner) := io.in(outer * 16 + middle * 4 + inner)//a[outer][middle][inner]
      eval.io.pt := io.pt0
      mid(outer * 4 + middle) := eval.io.out
    }
  }

  val high = Wire(Vec(4, UInt(outW.W)))
  for (outer <- 0 until 4) {
    val eval = Module(new EvalPoint(outW, outW))
    for (middle <- 0 until 4) eval.io.r(middle) := mid(outer * 4 + middle)
    eval.io.pt := io.pt1
    high(outer) := eval.io.out
  }

  val eval = Module(new EvalPoint(outW, outW))
  eval.io.r := high
  eval.io.pt := io.pt2
  io.out := eval.io.out
}

class EvalPoint(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r = Input(Vec(4, UInt(inW.W)))
    val pt = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  private def resize(value: UInt): UInt = {
    if (value.getWidth >= outW) value(outW - 1, 0)
    else Cat(0.U((outW - value.getWidth).W), value)
  }

  private def shift(value: UInt, amount: Int): UInt =
    ParaMath.mask(resize(value) << amount, outW)

  /*
   * Select the four signed power-of-two terms before the adder chain.  This
   * implements exactly one requested Toom point instead of building all seven
   * point datapaths and selecting one of their outputs.
   */
  val term0 = WireDefault(0.U(outW.W))
  val term1 = WireDefault(0.U(outW.W))
  val term2 = WireDefault(0.U(outW.W))
  val term3 = WireDefault(0.U(outW.W))
  val sub1 = WireDefault(false.B)
  val sub3 = WireDefault(false.B)

  switch(io.pt) {
    is(0.U) {
      term0 := resize(io.r(3))
    }
    is(1.U) {
      term0 := resize(io.r(0))
      term1 := shift(io.r(1), 1)
      term2 := shift(io.r(2), 2)
      term3 := shift(io.r(3), 3)
    }
    is(2.U) {
      term0 := resize(io.r(0))
      term1 := resize(io.r(1))
      term2 := resize(io.r(2))
      term3 := resize(io.r(3))
    }
    is(3.U) {
      term0 := resize(io.r(0))
      term1 := resize(io.r(1))
      term2 := resize(io.r(2))
      term3 := resize(io.r(3))
      sub1 := true.B
      sub3 := true.B
    }
    is(4.U) {
      term0 := shift(io.r(0), 3)
      term1 := shift(io.r(1), 2)
      term2 := shift(io.r(2), 1)
      term3 := resize(io.r(3))
    }
    is(5.U) {
      term0 := shift(io.r(0), 3)
      term1 := shift(io.r(1), 2)
      term2 := shift(io.r(2), 1)
      term3 := resize(io.r(3))
      sub1 := true.B
      sub3 := true.B
    }
    is(6.U) {
      term0 := resize(io.r(0))
    }
  }

  val sum01 = Wire(UInt(outW.W))
  val sum012 = Wire(UInt(outW.W))
  sum01 := Mux(
    sub1,
    ParaMath.mask(term0 -& term1, outW),
    ParaMath.mask(term0 +& term1, outW)
  )
  sum012 := ParaMath.mask(sum01 +& term2, outW)
  io.out := Mux(
    sub3,
    ParaMath.mask(sum012 -& term3, outW),
    ParaMath.mask(sum012 +& term3, outW)
  )
}
