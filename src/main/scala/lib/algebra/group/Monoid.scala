package vutils.algebra.group

import chisel3._

trait Monoid[T <: Data] extends SemiGroup[T] {
  // definition
  def identity: T

  // verification properties
  def identityLeft(x: T): Bool  = op(identity, x) === x
  def identityRight(x: T): Bool = op(x, identity) === x
}
