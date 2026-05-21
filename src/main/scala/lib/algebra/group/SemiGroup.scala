package vutils.algebra.group

import chisel3._

trait SemiGroup[T <: Data] {
  // definition
  def op(x: T, y: T): T

  // verification properties
  def assoc(x: T, y: T, z: T): Bool = op(op(x, y), z) === op(x, op(y, z))
}
