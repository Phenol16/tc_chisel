package High

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import java.time.LocalTime

class ToomCook1024Test extends AnyFlatSpec with ChiselScalatestTester {

  private def now(): String = LocalTime.now().toString

  private val N = 1024
  private val QMask: BigInt = (BigInt(1) << 24) - 1

  /**
    * negacyclic convolution modulo x^1024 + 1, then modulo 2^24.
    *
    * c[j] += a[i] * b[j - i]       if j >= i
    * c[j] -= a[i] * b[n + j - i]   if j < i
    */
  private def schoolbookNegacyclic(
      a: Seq[BigInt],
      b: Seq[BigInt],
      n: Int = N
  ): Seq[BigInt] = {
    val c = Array.fill(n)(BigInt(0))

    for (i <- 0 until n) {
      for (j <- i until n) {
        c(j) = c(j) + a(i) * b(j - i)
      }
      for (j <- 0 until i) {
        c(j) = c(j) - a(i) * b(n + j - i)
      }
    }

    c.map(x => x & QMask).toSeq
  }

  private def zeroVec: Seq[BigInt] = Seq.fill(N)(BigInt(0))

  private def oneHot(pos: Int, value: BigInt = BigInt(1)): Seq[BigInt] = {
    require(pos >= 0 && pos < N, s"oneHot position out of range: $pos")
    val arr = Array.fill(N)(BigInt(0))
    arr(pos) = value
    arr.toSeq
  }

  private def runCase(
      dut: ToomCook1024WithSram,
      label: String,
      aVals: Seq[BigInt],
      bVals: Seq[BigInt],
      expected: Seq[BigInt],
      maxWaitCycles: Int = 1200,
      maxPrintMismatch: Int = 30,
      printNonZero: Boolean = true
  ): Unit = {
    require(aVals.length == N, s"$label: a length must be $N")
    require(bVals.length == N, s"$label: b length must be $N")
    require(expected.length == N, s"$label: expected length must be $N")

    // addr = aa*4+bb.  The four c slices are physical SRAM banks; within a
    // bank the sixteen values are the sixteen core lanes.
    def packA(addr: Int, a: Seq[BigInt]): BigInt = {
      val aa = addr / 4
      val bb = addr % 4
      (0 until 4).flatMap { cc =>
        (0 until 16).map { lane =>
          val coeff = lane * 64 + aa * 16 + bb * 4 + cc
          (a(coeff) & ((BigInt(1) << 24) - 1)) << (cc * 16 * 24 + lane * 24)
        }
      }.sum
    }
    def packB(addr: Int, b: Seq[BigInt]): BigInt = {
      val aa = addr / 4
      val bb = addr % 4
      (0 until 4).flatMap { cc =>
        (0 until 16).map { lane =>
          val coeff = lane * 64 + aa * 16 + bb * 4 + cc
          (b(coeff) & ((BigInt(1) << 8) - 1)) << (cc * 16 * 8 + lane * 8)
        }
      }.sum
    }
    def unpackC(word: BigInt): Seq[BigInt] = {
      (0 until 64).map { i =>
        (word >> (24 * i)) & ((BigInt(1) << 24) - 1)
      }
    }

    println(s"[${now()}][$label] write input SRAM blocks")
    for (block <- 0 until 16) {
      dut.io.a_we.poke(true.B)
      dut.io.a_addr.poke(block.U)
      dut.io.a_din.poke(packA(block, aVals).U)
      dut.io.b_we.poke(true.B)
      dut.io.b_addr.poke(block.U)
      dut.io.b_din.poke(packB(block, bVals).U)
      dut.clock.step(1)
    }
    dut.io.a_we.poke(false.B)
    dut.io.b_we.poke(false.B)

    println(s"[${now()}][$label] send start")
    dut.io.start.poke(true.B)
    dut.clock.step(1)
    dut.io.start.poke(false.B)
    println(s"[${now()}][$label] start waiting done")

    var seenDone = false
    var cycle = 0
    while (!seenDone && cycle < maxWaitCycles) {
      seenDone = dut.io.done.peek().litToBoolean
      if (!seenDone) { dut.clock.step(1); cycle += 1 }
    }

    assert(
      seenDone,
      s"[$label] Error: 等待 io.done 超时！maxWaitCycles=$maxWaitCycles"
    )
    println(s"[${now()}][$label] done asserted at cycle=$cycle")
    println(s"[${now()}][$label] start checking outputs")

    var mismatchCount = 0
    val nonZero = scala.collection.mutable.ArrayBuffer[(Int, BigInt)]()

    val got = collection.mutable.ArrayBuffer[BigInt]()
    for (block <- 0 until 16) {
      dut.io.c_re.poke(true.B)
      dut.io.c_addr.poke(block.U)
      dut.clock.step(1)
      dut.io.c_re.poke(false.B)
      dut.io.c_valid.expect(true.B)
      got ++= unpackC(dut.io.c_dout.peek().litValue)
    }

    for (i <- 0 until N) {
      val g = got(i) & QMask
      val exp = expected(i) & QMask

      if (g != 0) {
        nonZero += ((i, g))
      }

      if (g != exp) {
        mismatchCount += 1
        if (mismatchCount <= maxPrintMismatch) {
          println(
            s"[$label MISMATCH] index=$i got=0x${g.toString(16)} expected=0x${exp.toString(16)}"
          )
        }
      }
    }

    if (printNonZero) {
      println(s"[${now()}][$label] nonzero output count=${nonZero.length}")
      nonZero.take(30).foreach { case (idx, value) =>
        println(s"[$label NONZERO] c[$idx] = 0x${value.toString(16)}")
      }
    }

    assert(
      mismatchCount == 0,
      s"[$label] 计算错误：共有 $mismatchCount 个系数不匹配，最多已打印 $maxPrintMismatch 个。"
    )

    println(s"[${now()}][$label] PASS, done cycle=$cycle")

    // 让 DUT 回到 IDLE，便于同一次 elaboration 内连续输入下一组 case
    dut.clock.step(2)
  }

  behavior of "ToomCook1024"

  it should "pass deterministic cases and full_random_a24_b8" in {
    println(s"[${now()}] before test(new ToomCook1024)")

    test(new ToomCook1024WithSram)
      .withAnnotations(Seq(
        VerilatorBackendAnnotation,
        VerilatorCFlags(Seq("-O3"))
      )) { dut =>

        dut.clock.setTimeout(0)

        println(s"[${now()}] after elaboration, simulation started")

        // ---------------------------------------------------------------------
        // Case 1: 全 0
        // ---------------------------------------------------------------------
        {
          val a = zeroVec
          val b = zeroVec
          val exp = zeroVec
          runCase(
            dut = dut,
            label = "zero",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 2: a(0)=1, b(0)=1
        // 理论：c(0)=1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(0)
          val b = oneHot(0)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a0_b0",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 3: a(1)=1, b(0)=1
        // 理论：c(1)=1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(1)
          val b = oneHot(0)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a1_b0",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 4: a(0)=1, b(1)=1
        // 理论：c(1)=1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(0)
          val b = oneHot(1)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a0_b1",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 5: a(1023)=1, b(1)=1
        // negacyclic: x^1023 * x = x^1024 = -1
        // 理论：c(0)=2^24-1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(1023)
          val b = oneHot(1)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a1023_b1_negacyclic",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 6: 小值随机
        // 如果 one-hot 通过但小值随机失败，多半是算法布局/插值问题。
        // ---------------------------------------------------------------------
        {
          val rng = new Random(7)
          val a = Seq.fill(N)(BigInt(rng.nextInt(16)))
          val b = Seq.fill(N)(BigInt(rng.nextInt(16)))
          val exp = schoolbookNegacyclic(a, b)

          runCase(
            dut = dut,
            label = "small_random_4bit",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = false
          )
        }

        // ---------------------------------------------------------------------
        // Case 7: 完整随机
        // 只有前面 case 都通过后，这个才有诊断意义。
        // ---------------------------------------------------------------------
        {
          val rng = new Random(42)
          val a = Seq.fill(N)(BigInt(rng.nextInt() & 0xffffff))
          val b = Seq.fill(N)(BigInt(rng.nextInt() & 0xff))
          val exp = schoolbookNegacyclic(a, b)

          runCase(
            dut = dut,
            label = "full_random_a24_b8",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = false
          )
        }

        println(s"[${now()}] all cases passed")
      }
  }
}
