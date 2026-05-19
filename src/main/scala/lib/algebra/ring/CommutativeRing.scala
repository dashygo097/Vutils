package vutils.algebra.ring

import vutils.algebra.group.Monoid
import chisel3._

trait MultiplicativeCommutativeMonoid[T <: Data] extends Monoid[T] {
  // definition
  def one: T             = identity
  def mul(x: T, y: T): T = op(x, y)

  // verification properties
  def commutative(x: T, y: T): Bool = op(x, y) === op(y, x)
}

trait CommutativeRing[T <: Data] {
  // definition
  def additive: AdditiveGroup[T]
  def multiplicative: MultiplicativeCommutativeMonoid[T]

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
