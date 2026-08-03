package High

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

private class Interp4ColsCoreHarness extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val in = Input(Vec(7 * 16, UInt(33.W)))

    val outValid = Output(Bool())
    val outChunk = Output(UInt(2.W))
    val selectedOut = Output(Vec(16, UInt(30.W)))
    val done = Output(Bool())
    val selectedNr = Output(Vec(3, UInt(30.W)))
  })

  val selected = Module(new Interp4ColsCoreEngine)
  val loadActive = RegInit(false.B)
  val loadPoint = RegInit(0.U(3.W))

  selected.io.inValid := io.start || loadActive
  selected.io.inPoint := Mux(io.start, 0.U, loadPoint)
  for (lane <- 0 until 16) {
    val point = Mux(io.start, 0.U, loadPoint)
    selected.io.inWord(lane) := io.in(point * 16.U + lane.U)
  }
  when(io.start) {
    loadActive := true.B
    loadPoint := 1.U
  }.elsewhen(loadActive) {
    when(loadPoint === 6.U) {
      loadActive := false.B
    }.otherwise {
      loadPoint := loadPoint + 1.U
    }
  }

  io.outValid := selected.io.outValid
  io.outChunk := selected.io.outChunk
  io.selectedOut := selected.io.out
  io.done := selected.io.done
  io.selectedNr := VecInit(
    selected.io.nr0,
    selected.io.nr1,
    selected.io.nr2
  )
}

private class Interp4ColsInteHarness extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val modeI2 = Input(Bool())
    val in = Input(Vec(7 * 16, UInt(30.W)))
    val pr0 = Input(UInt(27.W))
    val pr1 = Input(UInt(27.W))
    val pr2 = Input(UInt(27.W))

    val outValid = Output(Bool())
    val outChunk = Output(UInt(2.W))
    val selectedOut = Output(Vec(16, UInt(27.W)))
    val done = Output(Bool())
    val selectedNr = Output(Vec(3, UInt(27.W)))
  })

  val selected = Module(new Interp4ColsInteEngine)
  val loadActive = RegInit(false.B)
  val loadPoint = RegInit(0.U(3.W))

  selected.io.inValid := io.start || loadActive
  selected.io.inPoint := Mux(io.start, 0.U, loadPoint)
  for (lane <- 0 until 16) {
    val point = Mux(io.start, 0.U, loadPoint)
    selected.io.inWord(lane) := io.in(point * 16.U + lane.U)
  }
  selected.io.modeI2 := io.modeI2
  selected.io.pr0 := io.pr0
  selected.io.pr1 := io.pr1
  selected.io.pr2 := io.pr2
  when(io.start) {
    loadActive := true.B
    loadPoint := 1.U
  }.elsewhen(loadActive) {
    when(loadPoint === 6.U) {
      loadActive := false.B
    }.otherwise {
      loadPoint := loadPoint + 1.U
    }
  }

  io.outValid := selected.io.outValid
  io.outChunk := selected.io.outChunk
  io.selectedOut := selected.io.out
  io.done := selected.io.done
  io.selectedNr := VecInit(
    selected.io.nr0,
    selected.io.nr1,
    selected.io.nr2
  )
}

class Interp4ColsEngineTest extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "specialized four-column interpolation engines"

  private case class StepResult(
      out: Seq[BigInt],
      carry: Seq[BigInt]
  )

  private def mask(value: BigInt, width: Int): BigInt =
    value & ((BigInt(1) << width) - 1)

  private def step(
      input: Seq[BigInt],
      carry: Seq[BigInt],
      mode: Int
  ): StepResult = {
    val mk = Seq(33, 30, 27)(mode)
    val mk2 = Seq(30, 27, 24)(mode)
    val mk3 = Seq(31, 28, 25)(mode)
    def m(value: BigInt): BigInt = mask(value, mk)
    def m2(value: BigInt): BigInt = mask(value, mk2)
    def m3(value: BigInt): BigInt = mask(value, mk3)

    val p0 = m(input(0))
    val p1 = m(input(1))
    val p2 = m(input(2))
    val p3 = m(input(3))
    val p4 = m(input(4))
    val p5 = m(input(5))
    val p6 = m(input(6))
    val r5a = m(p5 - p4)
    val r3a = m(m(p3 - p2) >> 1)
    val r4a = m(p4 - p0)
    val r4b = m((r4a << 1) + r5a - (p6 << 7))
    val r2a = m(p2 + r3a)
    val r1a = m(p1 + p4 - (r2a << 6) - r2a)
    val r2b = m(r2a - p6 - p0)
    val r1b = m(
      r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5)
    )

    val inv3 = BigInt("2AAAAAAB", 16)
    val inv9 = BigInt("38E38E39", 16)
    val inv15 = BigInt("6EEEEEEF", 16)
    val r4c = m2((m(r4b - (r2b << 3)) >> 3) * inv3)
    val r5b = m3((m(r5a + r1b) >> 1) * inv15)
    val r1c = m3((m(r1b + (r3a << 4)) >> 1) * inv9)
    val r2c = m2(r2b - r4c)
    val r3b = m2(-r3a - r1c)
    val r5c = m2((r1c - r5b) >> 1)
    val r1d = m2(r1c - r5c)

    StepResult(
      out = Seq(
        m2(p6 + m2(carry(2))),
        m2(r5c + m2(carry(1))),
        m2(r4c + m2(carry(0))),
        r3b
      ),
      carry = Seq(m2(p0), r1d, r2c)
    )
  }

  private def reference(
      input: Seq[Seq[BigInt]],
      initialCarry: Seq[BigInt],
      mode: Int
  ): StepResult = {
    var carry = initialCarry
    val output = scala.collection.mutable.ArrayBuffer[BigInt]()
    for (column <- 0 until 16) {
      val result = step(
        (0 until 7).map(point => input(point)(column)),
        carry,
        mode
      )
      output ++= result.out
      carry = result.carry
    }
    StepResult(output.toSeq, carry)
  }

  it should "match pidx=1 with a fixed 33-to-30-bit Core engine" in {
    test(new Interp4ColsCoreHarness) { dut =>
      val rng = new Random(11)
      dut.io.start.poke(false.B)

      for (_ <- 0 until 6) {
        val input = Seq.fill(7)(Seq.fill(16)(BigInt(33, rng)))
        val expected = reference(input, Seq.fill(3)(BigInt(0)), 0)

        for (point <- 0 until 7; column <- 0 until 16) {
          dut.io.in(point * 16 + column).poke(input(point)(column).U)
        }

        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)
        dut.clock.step(6)

        for (chunk <- 0 until 4) {
          dut.io.outValid.expect(true.B)
          dut.io.outChunk.expect(chunk.U)
          for (index <- 0 until 16) {
            dut.io.selectedOut(index).expect(
              expected.out(chunk * 16 + index).U
            )
          }
          if (chunk == 3) {
            dut.io.done.expect(true.B)
            for (index <- 0 until 3) {
              dut.io.selectedNr(index).expect(
                expected.carry(index).U
              )
            }
          }
          dut.clock.step()
        }
        dut.io.outValid.expect(false.B)
      }
    }
  }

  it should "match pidx=2/3 with the shared 30-bit Inte engine" in {
    test(new Interp4ColsInteHarness) { dut =>
      val rng = new Random(17)
      dut.io.start.poke(false.B)

      for (mode <- 1 until 3; _ <- 0 until 5) {
        val input = Seq.fill(7)(Seq.fill(16)(BigInt(30, rng)))
        val initialCarry = Seq.fill(3)(BigInt(27, rng))
        val expected = reference(input, initialCarry, mode)

        dut.io.modeI2.poke((mode == 2).B)
        for (point <- 0 until 7; column <- 0 until 16) {
          dut.io.in(point * 16 + column).poke(input(point)(column).U)
        }
        dut.io.pr0.poke(initialCarry(0).U)
        dut.io.pr1.poke(initialCarry(1).U)
        dut.io.pr2.poke(initialCarry(2).U)

        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)
        dut.clock.step(6)

        for (chunk <- 0 until 4) {
          dut.io.outValid.expect(true.B)
          dut.io.outChunk.expect(chunk.U)
          for (index <- 0 until 16) {
            dut.io.selectedOut(index).expect(
              expected.out(chunk * 16 + index).U
            )
          }
          if (chunk == 3) {
            dut.io.done.expect(true.B)
            for (index <- 0 until 3) {
              dut.io.selectedNr(index).expect(
                expected.carry(index).U
              )
            }
          }
          dut.clock.step()
        }
        dut.io.outValid.expect(false.B)
      }
    }
  }

  it should "accept the next four point words while producing the current result" in {
    test(new Interp4ColsCoreEngine) { dut =>
      val rng = new Random(29)
      val inputs = Seq.fill(2)(
        Seq.fill(7)(Seq.fill(16)(BigInt(33, rng)))
      )
      val expected = inputs.map(input =>
        reference(input, Seq.fill(3)(BigInt(0)), 0)
      )

      dut.io.inValid.poke(false.B)
      dut.io.inPoint.poke(0.U)
      dut.io.inWord.foreach(_.poke(0.U))

      def drivePoint(group: Int, point: Int): Unit = {
        dut.io.inValid.poke(true.B)
        dut.io.inPoint.poke(point.U)
        for (lane <- 0 until 16) {
          dut.io.inWord(lane).poke(inputs(group)(point)(lane).U)
        }
      }

      for (point <- 0 until 7) {
        drivePoint(0, point)
        dut.clock.step()
      }

      for (chunk <- 0 until 4) {
        dut.io.outValid.expect(true.B)
        dut.io.outChunk.expect(chunk.U)
        for (lane <- 0 until 16) {
          dut.io.out(lane).expect(
            expected(0).out(chunk * 16 + lane).U
          )
        }
        drivePoint(1, chunk)
        dut.clock.step()
      }

      for (point <- 4 until 7) {
        drivePoint(1, point)
        dut.clock.step()
      }
      dut.io.inValid.poke(false.B)

      for (chunk <- 0 until 4) {
        dut.io.outValid.expect(true.B)
        dut.io.outChunk.expect(chunk.U)
        for (lane <- 0 until 16) {
          dut.io.out(lane).expect(
            expected(1).out(chunk * 16 + lane).U
          )
        }
        dut.clock.step()
      }
      dut.io.outValid.expect(false.B)
    }
  }

  it should "preserve the I0 patch when only the low 30 bits are stored" in {
    val rng = new Random(23)
    for (_ <- 0 until 1000) {
      val value = BigInt(33, rng)
      val carry = BigInt(30, rng)
      val oldLow30 = mask(mask(value - carry, 33), 30)
      val compact = mask(value - carry, 30)
      assert(oldLow30 == compact)
    }
  }
}
