package vutils.math.div

import chisel3._
import chisel3.util.{ Cat, MuxLookup }

class Radix4DivStepIO(val dw: Int, val extDw: Int) extends Bundle {
  val quotientIn      = Input(UInt(dw.W))
  val remainderIn     = Input(UInt((dw + 2).W))
  val dividendShiftIn = Input(UInt(extDw.W))
  val divisorAbsIn    = Input(UInt(dw.W))

  val qDigit            = Output(UInt(2.W))
  val nextQuotient      = Output(UInt(dw.W))
  val nextRemainder     = Output(UInt((dw + 2).W))
  val nextDividendShift = Output(UInt(extDw.W))
  val remainderMag      = Output(UInt(dw.W))
}

class Radix4DivStep(dw: Int) extends Module {
  require(dw > 0, "dw must be > 0")

  private val iterCount = (dw + 1) / 2
  private val extDw     = iterCount * 2

  override def desiredName: String = s"radix4_div_step_$dw"

  val io = IO(new Radix4DivStepIO(dw, extDw))

  private val partialRemainder =
    Cat(io.remainderIn(dw - 1, 0), io.dividendShiftIn(extDw - 1, extDw - 2))

  private val divisorExt = Cat(0.U(2.W), io.divisorAbsIn)
  private val divisorX2  = divisorExt << 1
  private val divisorX3  = divisorX2 + divisorExt

  private val ge3 = partialRemainder >= divisorX3
  private val ge2 = partialRemainder >= divisorX2
  private val ge1 = partialRemainder >= divisorExt

  private val qDigit =
    Mux(ge3, 3.U(2.W), Mux(ge2, 2.U(2.W), Mux(ge1, 1.U(2.W), 0.U(2.W))))

  private val nextRemainder =
    MuxLookup(qDigit, partialRemainder)(
      Seq(
        1.U -> (partialRemainder - divisorExt),
        2.U -> (partialRemainder - divisorX2),
        3.U -> (partialRemainder - divisorX3)
      )
    )

  private val quotientShifted   = (io.quotientIn << 2)(dw - 1, 0)
  private val nextQuotient      = quotientShifted | qDigit
  private val nextDividendShift = (io.dividendShiftIn << 2)(extDw - 1, 0)

  io.qDigit            := qDigit
  io.nextQuotient      := nextQuotient
  io.nextRemainder     := nextRemainder
  io.nextDividendShift := nextDividendShift
  io.remainderMag      := nextRemainder(dw - 1, 0)
}
