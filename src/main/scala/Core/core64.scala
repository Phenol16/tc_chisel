package core
import chisel3._
import chisel3.util._
class core64(
    t:Int=1,  //是否为top_Module
    k:Int=3,  //top_Module需要的分解层数
    sign:Int=1,
    aWidth: Int,
    bWidth: Int,
    evalGrowth:Int=4,
    inteGrowth:Int=3,
    cWidth:Int
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(64, UInt(aWidth.W)))
    val b = Input(Vec(64, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(64, UInt(cWidth.W)))
  })


  private val aEvalWidth = aWidth + (inteGrowth * k)*t
  private val bEvalWidth = bWidth + (evalGrowth * k + sign)*t
  private val InteWidth  = cWidth + inteGrowth


  val A_eval = Wire(Vec(7 * 16, UInt(aEvalWidth.W)))
  val B_eval = Wire(Vec(7 * 16, UInt(bEvalWidth.W)))
  for (j <- 0 until 16) {
  val evalA = Module(new Eval(inWidth = aWidth, outWidth = aEvalWidth))
  val evalB = Module(new Eval(inWidth = bWidth, outWidth = bEvalWidth))
    for (i <- 0 until 4) {
      evalA.io.in(i) := io.a(j * 4 + i)
      evalB.io.in(i) := io.b(j * 4 + i)
    }
    for (pt <- 0 until 7) {
      A_eval(pt * 16 + j) := evalA.io.out(pt)
      B_eval(pt * 16 + j) := evalB.io.out(pt)
    }
  }

  val core16 = Seq.fill(7)(
    Module(new core16(aWidth = aEvalWidth,bWidth = bEvalWidth,cWidth = InteWidth))
    )
  for (pt <- 0 until 7) {
    core16(pt).io.valid_in := io.valid_in
    for (i <- 0 until 16) {
      core16(pt).io.a(i) := A_eval(pt * 16 + i)
      core16(pt).io.b(i) := B_eval(pt * 16 + i)
    }
  }

  val core_valid = VecInit(core16.map(_.io.valid_out)).asUInt.andR
  val core_c_wire = Wire(Vec(7 * 16, UInt(InteWidth.W)))
  for (pt <- 0 until 7) {
    for (i <- 0 until 16) {
      core_c_wire(pt * 16 + i) := core16(pt).io.c(i)
    }
  }

  val s_valid = RegNext(core_valid, false.B)
  val s_w = RegNext(core_c_wire)

  val interp = Module(new Interpolation(
    stride = 16, 
    inWidth = InteWidth, 
    outWidth = cWidth))
    
  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}