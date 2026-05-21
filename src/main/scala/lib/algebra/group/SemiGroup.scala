package vutils.algebra.group

import chisel3._
import chisel3.util.Valid

trait SemiGroup[T <: Data] {
  // definition
  def op(x: T, y: T): T
  def vop(x: Valid[T], y: Valid[T]): Valid[T]

  // verification properties
  def vv(x: T, y: T): Bool = {
    val result = op(x, y)

    val vx = Wire(Valid(chiselTypeOf(x)))
    val vy = Wire(Valid(chiselTypeOf(y)))

    vx.bits  := x
    vx.valid := true.B

    vy.bits  := y
    vy.valid := true.B

    val vresult = vop(vx, vy)

    result === vresult.bits || !vresult.valid
  }

  def assoc(x: T, y: T, z: T): Bool = op(op(x, y), z) === op(x, op(y, z))
}
