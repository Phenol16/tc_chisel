package High

import chisel3._

object generator extends App {
  println("Generating the hardware")
  emitVerilog(new ToomCook1024()  , Array("--target-dir", "generated/ToomCook1024"))
}
