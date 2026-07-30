package core
import chisel3._

/** 4阶命名 wrapper */
class Core4Named(
    aW: Int,
    bW: Int,
    moduleName: String
) extends core4(
      aWidth = aW,
      bWidth = bW,
      cWidth = aW
    ) {
  override def desiredName: String = moduleName
}

/** 16阶命名 wrapper */
class Core16Named(
    aW: Int,
    bW: Int,
    moduleName: String
) extends core16(
      aWidth = aW,
      bWidth = bW,
      cWidth = aW
    ) {
  override def desiredName: String = moduleName
}

/** 64阶命名 wrapper */
class Core64Named(
    aW: Int,
    bW: Int,
    moduleName: String
) extends core64(
      aWidth = aW,
      bWidth = bW,
      cWidth = aW
    ) {
  override def desiredName: String = moduleName
}

object generator extends App {
  println("Generating parameterized Toom-Cook cores")

  val aWidths = Seq(24, 28, 32, 36)
  val bWidths = Seq(8, 10, 12, 14, 16)

  /*
   * 用法：
   *
   * 1. 生成全部 4/16/64：
   *    sbt "runMain core.generator"
   *
   * 2. 只生成 4 阶：
   *    sbt "runMain core.generator 4"
   *
   * 3. 只生成 16 阶：
   *    sbt "runMain core.generator 16"
   *
   * 4. 只生成 64 阶：
   *    sbt "runMain core.generator 64"
   *
   * 5. 生成多个指定阶数：
   *    sbt "runMain core.generator 4 64"
   *    sbt "runMain core.generator 4 16 64"
   */

  val selectedNs: Seq[Int] =
    if (args.isEmpty) Seq(4, 16, 64)
    else args.map(_.toInt).toSeq

  private def checkN(n: Int): Unit = {
    require(
      Seq(4, 16, 64).contains(n),
      s"Unsupported core size N=$n. Only 4, 16, 64 are supported."
    )
  }

  selectedNs.foreach(checkN)

  for {
    n <- selectedNs
    aW <- aWidths
    bW <- bWidths
  } {
    val cW = aW
    val name = s"core${n}_a${aW}_b${bW}_c${cW}"
    val targetDir = s"generated/core${n}/$name"

    println(s"Generating $name into $targetDir")

    n match {
      case 4 =>
        emitVerilog(
          new Core4Named(
            aW = aW,
            bW = bW,
            moduleName = name
          ),
          Array("--target-dir", targetDir)
        )

      case 16 =>
        emitVerilog(
          new Core16Named(
            aW = aW,
            bW = bW,
            moduleName = name
          ),
          Array("--target-dir", targetDir)
        )

      case 64 =>
        emitVerilog(
          new Core64Named(
            aW = aW,
            bW = bW,
            moduleName = name
          ),
          Array("--target-dir", targetDir)
        )
    }
  }

  println("All selected Verilog files generated.")
}