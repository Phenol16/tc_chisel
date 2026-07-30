package High

import chisel3._
import chiseltest._
import chiseltest.simulator.{
  TreadleBackendAnnotation,
  VerilatorBackendAnnotation,
  VerilatorCFlags
}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer

class ToomCook1024PipelineSmokeTest extends AnyFlatSpec with ChiselScalatestTester {
  private val mask24 = (BigInt(1) << 24) - 1

  private def packA(addr: Int, hot: Int): BigInt = {
    val aa = addr / 4
    val bb = addr % 4
    (0 until 4).flatMap { cc =>
      (0 until 16).map { lane =>
        val coeff = lane * 64 + aa * 16 + bb * 4 + cc
        (if (coeff == hot) BigInt(1) else BigInt(0)) << (cc * 384 + lane * 24)
      }
    }.sum
  }

  private def packB(addr: Int, hot: Int): BigInt = {
    val aa = addr / 4
    val bb = addr % 4
    (0 until 4).flatMap { cc =>
      (0 until 16).map { lane =>
        val coeff = lane * 64 + aa * 16 + bb * 4 + cc
        (if (coeff == hot) BigInt(1) else BigInt(0)) << (cc * 128 + lane * 8)
      }
    }.sum
  }

  behavior of "ToomCook1024 coarse SRAM pipeline"

  it should "accept a second task while the first task is in flight" in {
    val simulatorAnnotations =
      if (sys.props.get("toomcook.test.treadle").contains("true")) {
        Seq(TreadleBackendAnnotation)
      } else {
        Seq(
          VerilatorBackendAnnotation,
          VerilatorCFlags(Seq("-O3"))
        )
      }

    test(new ToomCook1024WithSram)
      .withAnnotations(simulatorAnnotations) { dut =>
        dut.clock.setTimeout(0)
        dut.io.start.poke(false.B)
        dut.io.a_we.poke(false.B)
        dut.io.b_we.poke(false.B)
        dut.io.c_re.poke(false.B)
        dut.io.a_addr.poke(0.U)
        dut.io.b_addr.poke(0.U)
        dut.io.c_addr.poke(0.U)
        dut.io.a_din.poke(0.U)
        dut.io.b_din.poke(0.U)

        var elapsed = 0
        def step(n: Int = 1): Unit = {
          dut.clock.step(n)
          elapsed += n
        }

        def enqueue(aHot: Int, bHot: Int): Unit = {
          while (dut.io.busy.peek().litToBoolean) {
            step()
          }
          dut.io.busy.expect(false.B)
          for (addr <- 0 until 16) {
            dut.io.a_we.poke(true.B)
            dut.io.b_we.poke(true.B)
            dut.io.a_addr.poke(addr.U)
            dut.io.b_addr.poke(addr.U)
            dut.io.a_din.poke(packA(addr, aHot).U)
            dut.io.b_din.poke(packB(addr, bHot).U)
            step()
          }
          dut.io.a_we.poke(false.B)
          dut.io.b_we.poke(false.B)
          dut.io.start.poke(true.B)
          step()
          dut.io.start.poke(false.B)
        }

        def waitDone(limit: Int): Int = {
          var waited = 0
          while (!dut.io.done.peek().litToBoolean && waited < limit) {
            step()
            waited += 1
          }
          assert(waited < limit, s"done timeout after $limit cycles")
          elapsed
        }

        def readResult(): Seq[BigInt] = {
          val result = ArrayBuffer[BigInt]()
          for (addr <- 0 until 16) {
            dut.io.c_re.poke(true.B)
            dut.io.c_addr.poke(addr.U)
            step()
            dut.io.c_re.poke(false.B)
            dut.io.c_valid.expect(true.B)
            val word = dut.io.c_dout.peek().litValue
            for (i <- 0 until 64) result += ((word >> (24 * i)) & mask24)
          }
          result.toSeq
        }

        // Task 0: 1 * 1 = 1.
        enqueue(aHot = 0, bHot = 0)
        // The single input SRAM is released after E0.  Task 1 is then loaded
        // while task 0 continues through E1/E2.
        // x^1023 * x = x^1024 = -1 modulo x^1024+1.
        enqueue(aHot = 1023, bHot = 1)

        val done0 = waitDone(1200)
        val result0 = readResult()
        assert(result0.head == 1 && result0.tail.forall(_ == 0))

        val done1 = waitDone(600)
        val result1 = readResult()
        assert(result1.head == mask24 && result1.tail.forall(_ == 0))

        val completionInterval = done1 - done0
        assert(
          completionInterval >= 343 && completionInterval <= 360,
          s"unexpected steady-state interval: $completionInterval"
        )
      }
  }
}
