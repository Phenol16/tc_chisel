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

  val layer = Module(new Eval(inWidth = inW, outWidth = outW))
  layer.io.in := io.r
  io.out := MuxLookup(io.pt, 0.U(outW.W))((0 until 7).map(i => i.U -> layer.io.out(i))) //根据 io.pt 的值，从 layer.io.out(0..6) 中选择一个作为 io.out
}