package vutils.math.mul

import chisel3._
import chisel3.util.MuxLookup

class BoothRadix4IO extends Bundle {
  val y    = Input(UInt(3.W))
  val neg  = Output(Bool())
  val zero = Output(Bool())
  val one  = Output(Bool())
  val two  = Output(Bool())
}

class BoothRadix4 extends Module {
  override def desiredName: String = "booth_radix4"

  val io = IO(new BoothRadix4IO)

  private val zeroCode = "b0100".U(4.W)
  private val plusOne  = "b0010".U(4.W)
  private val plusTwo  = "b0001".U(4.W)
  private val negOne   = "b1010".U(4.W)
  private val negTwo   = "b1001".U(4.W)

  private val encoding = MuxLookup(io.y, zeroCode)(
    Seq(
      0.U -> zeroCode, // 000:  0
      1.U -> plusOne,  // 001: +1
      2.U -> plusOne,  // 010: +1
      3.U -> plusTwo,  // 011: +2
      4.U -> negTwo,   // 100: -2
      5.U -> negOne,   // 101: -1
      6.U -> negOne,   // 110: -1
      7.U -> zeroCode  // 111:  0
    )
  )

  io.neg  := encoding(3)
  io.zero := encoding(2)
  io.one  := encoding(1)
  io.two  := encoding(0)
}
