package vutils.algebra.group

import chisel3._

trait Group[T <: Data] extends Monoid[T] {
  // definition
  def inv(x: T): T

  // verification properties
  def inverseLeft(x: T): Bool  = op(inv(x), x) === identity
  def inverseRight(x: T): Bool = op(x, inv(x)) === identity
}
