package core
import chisel3._
import chisel3.util._
object MagicNumber {
  private def modulus(width: Int): BigInt = BigInt(1) << width
  def inverseOdd(value: Int, width: Int): BigInt = {
    BigInt(value).modInverse(modulus(width))
  }
  def inv3(width: Int): BigInt = inverseOdd(3, width)
  def inv9(width: Int): BigInt = inverseOdd(9, width)
  def inv15(width: Int): BigInt = inverseOdd(15, width)
}

object ParaMath {
  def mask(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }
}