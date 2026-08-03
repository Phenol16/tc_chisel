package High

import chisel3._
import chisel3.util._
import core._

private class RfSpMacro(width: Int, depth: Int) extends BlackBox {
  private val supported =
    (depth == 28 && (width == 160 || width == 144)) ||
      (depth == 148 && width == 160) ||
      (depth == 196 &&
        (width == 160 || width == 144 || width == 132)) ||
      (depth == 112 && width == 144)
  require(supported, s"unsupported exact RF macro: ${depth}x${width}")

  override def desiredName: String = s"RSPHVT${depth}X${width}"
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val cen = Input(Bool())
    val wen = Input(Bool())
    val a = Input(UInt(log2Ceil(depth).W))
    val d = Input(UInt(width.W))
    val q = Output(UInt(width.W))
    val ema = Input(UInt(3.W))
    val emaw = Input(UInt(2.W))
    val emas = Input(Bool())
    val ret1n = Input(Bool())
  })
}

class SpRam(width: Int, depth: Int, useMemoryCompiler: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val en = Input(Bool())
    val we = Input(Bool())
    val addr = Input(UInt(log2Ceil(depth).W))
    val din = Input(UInt(width.W))
    val dout = Output(UInt(width.W))
  })

  if (!useMemoryCompiler) {
    val mem = withClock(io.clk) { SyncReadMem(depth, UInt(width.W)) }
    val readData = withClock(io.clk) { mem.read(io.addr, io.en && !io.we) }
    withClock(io.clk) {
      when(io.en && io.we) { mem.write(io.addr, io.din) }
    }
    io.dout := readData
  } else {
    val macroRam = Module(new RfSpMacro(width, depth))
    macroRam.io.clk := io.clk
    macroRam.io.cen := !io.en
    macroRam.io.wen := !io.we
    macroRam.io.a := io.addr
    macroRam.io.d := io.din
    macroRam.io.ema := "b011".U
    macroRam.io.emaw := "b01".U
    macroRam.io.emas := false.B
    macroRam.io.ret1n := true.B
    io.dout := macroRam.io.q
  }
}

/**
  * One logical 1RW memory implemented by compiler-friendly narrow 1RW macros.
  *
  * All stripes share one address and one enable/write command.  This is not a
  * multi-port RAM: the stripes only provide the aggregate word bandwidth that
  * is required by one logical access.
  */
class StripedSpRam(
    width: Int,
    depth: Int,
    maxStripeWidth: Int,
    useMemoryCompiler: Boolean
)
    extends Module {
  require(width > 0)
  require(depth > 0)
  require(maxStripeWidth > 0 && maxStripeWidth <= 160)

  val io = IO(new Bundle {
    val clk = Input(Clock())
    val en = Input(Bool())
    val we = Input(Bool())
    val addr = Input(UInt(log2Ceil(depth).W))
    val din = Input(UInt(width.W))
    val dout = Output(UInt(width.W))
  })

  private val stripeCount =
    (width + maxStripeWidth - 1) / maxStripeWidth
  private val stripes = (0 until stripeCount).map { index =>
    val low = index * maxStripeWidth
    val stripeWidth = math.min(maxStripeWidth, width - low)
    Module(new SpRam(stripeWidth, depth, useMemoryCompiler))
  }

  val readParts = Wire(Vec(stripeCount, UInt(maxStripeWidth.W)))
  for (index <- 0 until stripeCount) {
    val low = index * maxStripeWidth
    val stripeWidth = math.min(maxStripeWidth, width - low)
    val high = low + stripeWidth - 1
    val ram = stripes(index)

    ram.io.clk := io.clk
    ram.io.en := io.en
    ram.io.we := io.we
    ram.io.addr := io.addr
    ram.io.din := io.din(high, low)

    readParts(index) := 0.U
    readParts(index) := ram.io.dout
  }

  io.dout := Cat(
    (0 until stripeCount).reverse.map { index =>
      val low = index * maxStripeWidth
      val stripeWidth = math.min(maxStripeWidth, width - low)
      readParts(index)(stripeWidth - 1, 0)
    }
  )
}

/**
  * A and B use the same address and command, but are stored in independent
  * physical RF macro stripes so no macro mixes A and B bits.
  */
class EvalPairSpRam(
    aWidth: Int,
    bWidth: Int,
    depth: Int,
    useMemoryCompiler: Boolean
) extends Module {
  private val pairWidth = aWidth + bWidth

  val io = IO(new Bundle {
    val clk = Input(Clock())
    val en = Input(Bool())
    val we = Input(Bool())
    val addr = Input(UInt(log2Ceil(depth).W))
    val din = Input(UInt(pairWidth.W))
    val dout = Output(UInt(pairWidth.W))
  })

  val aRam =
    Module(new StripedSpRam(aWidth, depth, 160, useMemoryCompiler))
  val bRam =
    Module(new StripedSpRam(bWidth, depth, 160, useMemoryCompiler))

  aRam.io.clk := io.clk
  aRam.io.en := io.en
  aRam.io.we := io.we
  aRam.io.addr := io.addr
  aRam.io.din := io.din(aWidth - 1, 0)

  bRam.io.clk := io.clk
  bRam.io.en := io.en
  bRam.io.we := io.we
  bRam.io.addr := io.addr
  bRam.io.din := io.din(pairWidth - 1, aWidth)

  io.dout := Cat(bRam.io.dout, aRam.io.dout)
}
