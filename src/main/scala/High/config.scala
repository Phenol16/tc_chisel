package High
import chisel3._
import chisel3.util._
object EvalWidth {
  val A_EVAL_W = 39
  val B_EVAL_W = 29
}

object EvalStorageWidth {
  val A_E0_W = 29
  val B_E0_W = 13
  val A_E1_W = 33
  val B_E1_W = 17
  val A_E2_W = 37
  val B_E2_W = 21
}

object InterpStorageWidth {
  val CORE_INPUT_W = 33
  val I0_W = 30
  val I1_W = 27
  val I2_W = 24
}

object InterpParamTable {
  case class Param(mk: Int, mk2: Int, mk3: Int, inv3: BigInt, inv9: BigInt, inv15: BigInt)

  val params = Seq(
    Param(36, 33, 34, BigInt("AAAAAAAAB", 16), BigInt("238E38E39", 16), BigInt("2EEEEEEEF", 16)),
    Param(33, 30, 31, BigInt("2AAAAAAB", 16), BigInt("38E38E39", 16), BigInt("6EEEEEEF", 16)),
    Param(30, 27, 28, BigInt("2AAAAAB", 16), BigInt("8E38E39", 16), BigInt("EEEEEEF", 16)),
    Param(27, 24, 25, BigInt("AAAAAB", 16), BigInt("E38E39", 16), BigInt("EEEEEEF", 16))
  )
}
