package vutils.algebra.group

import chisel3._

trait AbelianGroup[T <: Data] extends Group[T] {
  // definition

  // verification properties
  def commutative(x: T, y: T): Bool = op(x, y) === op(y, x)
}
