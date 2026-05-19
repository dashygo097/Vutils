package vutils.algebra.ring

import vutils.algebra.group.{ AbelianGroup, Monoid }
import chisel3._

trait AdditiveGroup[T <: Data] extends AbelianGroup[T] {
  // definition
  def zero: T            = identity
  def add(x: T, y: T): T = op(x, y)
  def neg(x: T): T       = inv(x)
}

trait MultiplicativeMonoid[T <: Data] extends Monoid[T] {
  // definition
  def one: T             = identity
  def mul(x: T, y: T): T = op(x, y)
}

trait Ring[T <: Data] {
  // definition
  def additive: AdditiveGroup[T]
  def multiplicative: MultiplicativeMonoid[T]

  // verification properties
  def distributiveLeft(x: T, y: T, z: T): Bool  = {
    val add = additive.add _
    val mul = multiplicative.mul _
    mul(x, add(y, z)) === add(mul(x, y), mul(x, z))
  }
  def distributiveRight(x: T, y: T, z: T): Bool = {
    val add = additive.add _
    val mul = multiplicative.mul _
    mul(add(x, y), z) === add(mul(x, z), mul(y, z))
  }
}
