package vutils.math.div

import chisel3._
import chisel3.util.{ Cat, Fill, Decoupled, switch, is, log2Ceil }

class DividerReq(val dw: Int) extends Bundle {
  val dividend = UInt(dw.W)
  val divisor  = UInt(dw.W)

  val signed          = Bool()
  val selectRemainder = Bool()
}

class DividerResp(val dw: Int) extends Bundle {
  val result    = UInt(dw.W)
  val quotient  = UInt(dw.W)
  val remainder = UInt(dw.W)

  val divByZero = Bool()
}

class RestoringDividerIO(val dw: Int) extends Bundle {
  val in   = Flipped(Decoupled(new DividerReq(dw)))
  val out  = Decoupled(new DividerResp(dw))
  val kill = Input(Bool())
  val busy = Output(Bool())
}

object RestoringDividerState extends ChiselEnum {
  val IDLE, RUN, DONE = Value
}

class RestoringDivider(dw: Int) extends Module {
  require(dw > 0, "dw must be > 0")

  private val iterCount      = (dw + 1) / 2
  private val extDw          = iterCount * 2
  private val stepCountWidth = math.max(1, log2Ceil(iterCount + 1))

  override def desiredName: String = s"radix4_restoring_divider_$dw"

  val io = IO(new RestoringDividerIO(dw))

  private def negate(value: UInt): UInt =
    (~value).asUInt + 1.U

  private def absWithNegFlag(value: UInt, neg: Bool): UInt =
    Mux(neg, negate(value), value)

  private def extendDividend(value: UInt): UInt =
    if (extDw == dw) value else Cat(0.U((extDw - dw).W), value)

  private val state = RegInit(RestoringDividerState.IDLE)

  private val stepCountReg     = RegInit(0.U(stepCountWidth.W))
  private val quotientReg      = RegInit(0.U(dw.W))
  private val remainderReg     = RegInit(0.U((dw + 2).W))
  private val dividendShiftReg = RegInit(0.U(extDw.W))
  private val divisorAbsReg    = RegInit(0.U(dw.W))

  private val quotientNegReg     = RegInit(false.B)
  private val remainderNegReg    = RegInit(false.B)
  private val selectRemainderReg = RegInit(false.B)

  private val quotientOutReg  = RegInit(0.U(dw.W))
  private val remainderOutReg = RegInit(0.U(dw.W))
  private val divByZeroReg    = RegInit(false.B)

  private val step = Module(new Radix4DivStep(dw))
  step.io.quotientIn      := quotientReg
  step.io.remainderIn     := remainderReg
  step.io.dividendShiftIn := dividendShiftReg
  step.io.divisorAbsIn    := divisorAbsReg

  private val req = io.in.bits

  private val divisorIsZero = req.divisor === 0.U
  private val dividendNeg   = req.signed && req.dividend(dw - 1)
  private val divisorNeg    = req.signed && req.divisor(dw - 1)

  private val minInt   = (BigInt(1) << (dw - 1)).U(dw.W)
  private val minusOne = ((BigInt(1) << dw) - 1).U(dw.W)

  private val signedOverflow =
    req.signed && req.dividend === minInt && req.divisor === minusOne

  private val dividendAbs = absWithNegFlag(req.dividend, dividendNeg)
  private val divisorAbs  = absWithNegFlag(req.divisor, divisorNeg)

  private val startFire = io.in.fire
  private val outFire   = io.out.fire
  private val lastStep  = stepCountReg === (iterCount - 1).U

  io.in.ready := state === RestoringDividerState.IDLE && !io.kill

  io.out.valid          := state === RestoringDividerState.DONE && !io.kill
  io.out.bits.quotient  := quotientOutReg
  io.out.bits.remainder := remainderOutReg
  io.out.bits.result    := Mux(selectRemainderReg, remainderOutReg, quotientOutReg)
  io.out.bits.divByZero := divByZeroReg

  io.busy := state =/= RestoringDividerState.IDLE

  when(io.kill) {
    state              := RestoringDividerState.IDLE
    stepCountReg       := 0.U
    quotientReg        := 0.U
    remainderReg       := 0.U
    dividendShiftReg   := 0.U
    divisorAbsReg      := 0.U
    quotientNegReg     := false.B
    remainderNegReg    := false.B
    selectRemainderReg := false.B
    quotientOutReg     := 0.U
    remainderOutReg    := 0.U
    divByZeroReg       := false.B
  }.otherwise {
    switch(state) {
      is(RestoringDividerState.IDLE) {
        when(startFire) {
          selectRemainderReg := req.selectRemainder

          when(divisorIsZero) {
            quotientOutReg  := Fill(dw, 1.U(1.W))
            remainderOutReg := req.dividend
            divByZeroReg    := true.B
            state           := RestoringDividerState.DONE
          }.elsewhen(signedOverflow) {
            quotientOutReg  := minInt
            remainderOutReg := 0.U
            divByZeroReg    := false.B
            state           := RestoringDividerState.DONE
          }.otherwise {
            quotientReg      := 0.U
            remainderReg     := 0.U
            dividendShiftReg := extendDividend(dividendAbs)
            divisorAbsReg    := divisorAbs

            quotientNegReg  := dividendNeg ^ divisorNeg
            remainderNegReg := dividendNeg
            divByZeroReg    := false.B
            stepCountReg    := 0.U

            state := RestoringDividerState.RUN
          }
        }
      }

      is(RestoringDividerState.RUN) {
        quotientReg      := step.io.nextQuotient
        remainderReg     := step.io.nextRemainder
        dividendShiftReg := step.io.nextDividendShift

        when(lastStep) {
          quotientOutReg :=
            Mux(quotientNegReg, negate(step.io.nextQuotient), step.io.nextQuotient)

          remainderOutReg :=
            Mux(remainderNegReg, negate(step.io.remainderMag), step.io.remainderMag)

          stepCountReg := 0.U
          state        := RestoringDividerState.DONE
        }.otherwise {
          stepCountReg := stepCountReg + 1.U
        }
      }

      is(RestoringDividerState.DONE) {
        when(outFire) {
          state := RestoringDividerState.IDLE
        }
      }
    }
  }
}
