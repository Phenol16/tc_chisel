package High

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

private class Eval64CompactPath(
    inW: Int,
    fullW: Int,
    e0W: Int,
    e1W: Int,
    e1BaseW: Int,
    sharedW: Int,
    e2W: Int
) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(64, UInt(inW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val out = Output(UInt(fullW.W))
  })

  val e0Extended = Wire(Vec(16, UInt(sharedW.W)))
  for (group <- 0 until 16) {
    val eval = Module(new EvalPoint(sharedW, sharedW))
    for (part <- 0 until 4) {
      eval.io.r(part) :=
        Cat(0.U((sharedW - inW).W), io.in(group * 4 + part))
    }
    eval.io.pt := io.pt0
    val stored = eval.io.out(e0W - 1, 0)
    e0Extended(group) :=
      Cat(Fill(sharedW - e0W, stored(e0W - 1)), stored)
    assert(
      eval.io.out(sharedW - 1, e0W) ===
        Fill(sharedW - e0W, eval.io.out(e0W - 1)),
      "E0 compact width overflow"
    )
  }

  val e1Extended = Wire(Vec(4, UInt(e2W.W)))
  val pt0Wide = io.pt0 === 1.U || io.pt0 === 4.U || io.pt0 === 5.U
  val pt1Wide = io.pt1 === 1.U || io.pt1 === 4.U || io.pt1 === 5.U
  val pt0Four = io.pt0 === 2.U || io.pt0 === 3.U
  val pt1Four = io.pt1 === 2.U || io.pt1 === 3.U
  val needsExtension =
    pt0Wide || pt1Wide || (pt0Four && pt1Four)
  for (group <- 0 until 4) {
    val eval = Module(new EvalPoint(sharedW, sharedW))
    for (part <- 0 until 4) {
      eval.io.r(part) := e0Extended(group * 4 + part)
    }
    eval.io.pt := io.pt1
    val full = eval.io.out(e1W - 1, 0)
    val base = full(e1BaseW - 1, 0)
    val stored = Mux(
      needsExtension,
      full,
      Cat(Fill(e1W - e1BaseW, base(e1BaseW - 1)), base)
    )
    when(!needsExtension) {
      assert(
        full(e1W - 1, e1BaseW) ===
          Fill(e1W - e1BaseW, base(e1BaseW - 1)),
        "E1 base-only row overflow"
      )
    }
    e1Extended(group) :=
      Cat(Fill(e2W - e1W, stored(e1W - 1)), stored)
  }

  val eval = Module(new EvalPoint(e2W, e2W))
  eval.io.r := e1Extended
  eval.io.pt := io.pt2
  io.out :=
    Cat(Fill(fullW - e2W, eval.io.out(e2W - 1)), eval.io.out)
}

private class EvalStorageWidthHarness extends Module {
  val io = IO(new Bundle {
    val a = Input(Vec(64, UInt(24.W)))
    val b = Input(Vec(64, UInt(8.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val compactA = Output(UInt(EvalWidth.A_EVAL_W.W))
    val compactB = Output(UInt(EvalWidth.B_EVAL_W.W))
  })

  val compactA = Module(new Eval64CompactPath(
    24,
    EvalWidth.A_EVAL_W,
    EvalStorageWidth.A_E0_W,
    EvalStorageWidth.A_E1_W,
    27,
    EvalStorageWidth.A_E1_W,
    EvalStorageWidth.A_E2_W
  ))
  val compactB = Module(new Eval64CompactPath(
    8,
    EvalWidth.B_EVAL_W,
    EvalStorageWidth.B_E0_W,
    EvalStorageWidth.B_E1_W,
    13,
    EvalStorageWidth.B_E1_W,
    EvalStorageWidth.B_E2_W
  ))
  compactA.io.in := io.a
  compactB.io.in := io.b

  compactA.io.pt0 := io.pt0
  compactA.io.pt1 := io.pt1
  compactA.io.pt2 := io.pt2
  compactB.io.pt0 := io.pt0
  compactB.io.pt1 := io.pt1
  compactB.io.pt2 := io.pt2
  io.compactA := compactA.io.out
  io.compactB := compactB.io.out
}

class EvalStorageWidthTest extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "compact E0/E1 Eval storage"

  it should "match the full-width three-layer Eval" in {
    test(new EvalStorageWidthHarness) { dut =>
      val aMask = (BigInt(1) << 24) - 1
      val bMask = (BigInt(1) << 8) - 1
      val rng = new Random(31)

      def eval4(values: Seq[BigInt], width: Int, point: Int): BigInt = {
        val mask = (BigInt(1) << width) - 1
        val r = values.map(_ & mask)
        val result = point match {
          case 0 => r(3)
          case 1 => r(0) + (r(1) << 1) + (r(2) << 2) + (r(3) << 3)
          case 2 => r(0) + r(1) + r(2) + r(3)
          case 3 => r(0) - r(1) + r(2) - r(3)
          case 4 => (r(0) << 3) + (r(1) << 2) + (r(2) << 1) + r(3)
          case 5 => (r(0) << 3) - (r(1) << 2) + (r(2) << 1) - r(3)
          case 6 => r(0)
        }
        result & mask
      }

      def eval64(
          values: Seq[BigInt],
          width: Int,
          pt0: Int,
          pt1: Int,
          pt2: Int
      ): BigInt = {
        val mid = (0 until 16).map { group =>
          eval4(values.slice(group * 4, group * 4 + 4), width, pt0)
        }
        val high = (0 until 4).map { group =>
          eval4(mid.slice(group * 4, group * 4 + 4), width, pt1)
        }
        eval4(high, width, pt2)
      }

      def run(
          aValues: Seq[BigInt],
          bValues: Seq[BigInt],
          pt0: Int,
          pt1: Int,
          pt2: Int
      ): Unit = {
        for (index <- 0 until 64) {
          dut.io.a(index).poke(aValues(index).U)
          dut.io.b(index).poke(bValues(index).U)
        }
        dut.io.pt0.poke(pt0.U)
        dut.io.pt1.poke(pt1.U)
        dut.io.pt2.poke(pt2.U)
        dut.clock.step()
        dut.io.compactA.expect(
          eval64(aValues, EvalWidth.A_EVAL_W, pt0, pt1, pt2).U
        )
        dut.io.compactB.expect(
          eval64(bValues, EvalWidth.B_EVAL_W, pt0, pt1, pt2).U
        )
      }

      val directedPoints =
        ((0 until 7).map(point => (point, 1, 1)) ++
          (0 until 7).map(point => (1, point, 1)) ++
          (0 until 7).map(point => (1, 1, point)) ++
          Seq((3, 3, 3), (5, 5, 5), (1, 3, 5), (5, 1, 3))).distinct

      for ((pt0, pt1, pt2) <- directedPoints) {
        run(
          Seq.fill(64)(aMask),
          Seq.fill(64)(bMask),
          pt0,
          pt1,
          pt2
        )
      }

      run(
        Seq.fill(64)(BigInt(0)),
        Seq.fill(64)(BigInt(0)),
        0,
        0,
        0
      )

      for (_ <- 0 until 20) {
        val aValues = Seq.fill(64)(BigInt(24, rng))
        val bValues = Seq.fill(64)(BigInt(8, rng))
        run(
          aValues,
          bValues,
          rng.nextInt(7),
          rng.nextInt(7),
          rng.nextInt(7)
        )
      }
    }
  }
}
