package High

import chisel3._
import chiseltest._
import core.Eval
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

private class EvalPointEquivalenceHarness(inW: Int, outW: Int)
    extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(4, UInt(inW.W)))
    val point = Input(UInt(3.W))
    val selected = Output(UInt(outW.W))
    val reference = Output(UInt(outW.W))
  })

  val selected = Module(new EvalPoint(inW, outW))
  val reference = Module(new Eval(inW, outW))
  selected.io.r := io.in
  selected.io.pt := io.point
  reference.io.in := io.in

  io.selected := selected.io.out
  io.reference := reference.io.out(io.point)
}

class EvalPointSelectedTest extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "EvalPoint selected-point datapath"

  private def runWidth(inW: Int, outW: Int, seed: Int): Unit = {
    test(new EvalPointEquivalenceHarness(inW, outW)) { dut =>
      val inMask = (BigInt(1) << inW) - 1
      val corners = Seq(
        Seq.fill(4)(BigInt(0)),
        Seq.fill(4)(inMask),
        Seq(BigInt(0), inMask, BigInt(0), inMask),
        Seq(inMask, BigInt(0), inMask, BigInt(0))
      )
      val rng = new Random(seed)
      val randoms = Seq.fill(200) {
        Seq.fill(4)(BigInt(inW, rng))
      }

      for (values <- corners ++ randoms; point <- 0 until 7) {
        for (index <- 0 until 4) {
          dut.io.in(index).poke(values(index).U)
        }
        dut.io.point.poke(point.U)
        dut.clock.step()
        dut.io.selected.expect(dut.io.reference.peek())
      }
    }
  }

  it should "match the seven-output Eval for A and B widths" in {
    runWidth(39, 39, seed = 1)
    runWidth(29, 29, seed = 2)
    runWidth(24, 39, seed = 3)
    runWidth(8, 29, seed = 4)
  }
}
