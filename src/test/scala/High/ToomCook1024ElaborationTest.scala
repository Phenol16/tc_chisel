package High

import chisel3.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.{Files, Path}
import java.util.Comparator

class ToomCook1024ElaborationTest extends AnyFlatSpec {
  behavior of "ToomCook1024 memory-compiler top"

  it should "elaborate with the compressed E1 macro set" in {
    val targetDir = Files.createTempDirectory("toomcook1024-elaboration-")
    try {
      val verilog =
        (new ChiselStage).emitVerilog(
          new ToomCook1024(useMemoryCompiler = true),
          Array("--target-dir", targetDir.toString)
        )

      assert(verilog.contains("RSPHVT196X160"))
      assert(verilog.contains("RSPHVT148X160"))
      assert(verilog.contains("RSPHVT112X144"))
    } finally {
      val paths = Files.walk(targetDir)
      try {
        paths
          .sorted(Comparator.reverseOrder[Path]())
          .forEach((path: Path) => Files.deleteIfExists(path))
      } finally {
        paths.close()
      }
    }
  }
}
