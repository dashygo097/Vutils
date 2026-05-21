package examples

import vutils._
import vutils.algebra.group.Monoid
import vutils.pipe.Tree
import chisel3._
import chisel3.util.{ Valid, ShiftRegister }

trait UInt32AddMonoid extends Monoid[UInt] with Tree[UInt] {
  override def identity: UInt =
    0.U(32.W)

  override def op(x: UInt, y: UInt): UInt =
    x +% y

  override def vop(x: Valid[UInt], y: Valid[UInt]): Valid[UInt] = {
    val v = Wire(Valid(UInt(32.W)))

    v.bits  := ShiftRegister(op(x.bits, y.bits), 3)
    v.valid := ShiftRegister(x.valid && y.valid, 3)

    v
  }
}

class UInt32Add4 extends Module with UInt32AddMonoid {
  val io = IO(new Bundle {
    val in0 = Input(UInt(32.W))
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val in3 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  io.out := treeReducePipe(
    Seq(io.in0, io.in1, io.in2, io.in3),
    interval = 1
  )
}

object TreeReducedAdder extends App {
  DesignEmitter.emit(
    gen = new UInt32Add4,
    filename = "tree_reduced_adder",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
