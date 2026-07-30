package core
import chisel3._
import chisel3.util._
class core4(
    t:Int=0,  //是否为top_Module
    k:Int=1,
    sign:Int=1,
    aWidth: Int,
    bWidth: Int,
    evalGrowth:Int=4,
    inteGrowth:Int=3,
    cWidth:Int
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(4, UInt(aWidth.W)))
    val b = Input(Vec(4, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4, UInt(cWidth.W)))
  })

  private val aEvalWidth = aWidth + (inteGrowth * k)*t
  private val bEvalWidth = bWidth + (evalGrowth * k + sign)*t
  private val InteWidth  = cWidth + inteGrowth


  val evalA = Module(new Eval(inWidth = aWidth, outWidth = aEvalWidth))
  val evalB = Module(new Eval(inWidth = bWidth, outWidth = bEvalWidth))
  evalA.io.in := io.a
  evalB.io.in := io.b

  val w = Wire(Vec(7, UInt(InteWidth.W)))
  for (i <- 0 until 7) {
    val aSigned = evalA.io.out(i).asSInt
    val bSigned = evalB.io.out(i).asSInt
    w(i) := (aSigned *bSigned).asUInt
  }

  val s_valid = RegNext(io.valid_in, false.B)
  val s_w = RegNext(w)

  val interp = Module(new Interpolation(
  stride = 1, 
  inWidth = InteWidth,  
  outWidth = cWidth))

  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}