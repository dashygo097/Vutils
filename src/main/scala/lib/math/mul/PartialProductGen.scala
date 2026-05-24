package vutils.math.mul

import chisel3._
import chisel3.util.{ Cat, Fill }

class PartialProductGenIO(val operandWidth: Int, val productWidth: Int) extends Bundle {
  val multiplicand = Input(UInt(operandWidth.W))
  val multiplier   = Input(UInt(operandWidth.W))

  val multiplicandSigned = Input(Bool())
  val multiplierSigned   = Input(Bool())

  val partialProducts = Output(Vec((operandWidth + 1) / 2, UInt(productWidth.W)))
}

class PartialProductGen(val operandWidth: Int, val productWidth: Int) extends Module {
  require(operandWidth > 0, "operandWidth must be > 0")
  require(productWidth > 0, "productWidth must be > 0")

  override def desiredName: String =
    s"partial_product_gen_${operandWidth}_to_$productWidth"

  val io = IO(new PartialProductGenIO(operandWidth, productWidth))

  private val groupCount = (operandWidth + 1) / 2
  private val rawWidth   = operandWidth + 2

  private def signExtend(value: UInt, width: Int): UInt =
    if (width <= value.getWidth) {
      value(width - 1, 0)
    } else {
      Cat(Fill(width - value.getWidth, value(value.getWidth - 1)), value)
    }

  private def twosComplement(value: UInt): UInt = (~value).asUInt + 1.U

  private val multiplierSign = Mux(io.multiplierSigned, io.multiplier(operandWidth - 1), 0.U(1.W))
  private val multiplierExt  = Cat(Fill(2, multiplierSign), io.multiplier, 0.U(1.W))

  private val multiplicandSign = Mux(io.multiplicandSigned, io.multiplicand(operandWidth - 1), 0.U(1.W))

  private val multiplicandX1 = Cat(Fill(2, multiplicandSign), io.multiplicand)

  private val multiplicandX2 = Cat(Fill(1, multiplicandSign), io.multiplicand, 0.U(1.W))

  private val encoders = Seq.fill(groupCount)(Module(new BoothRadix4))

  for (i <- 0 until groupCount) {
    val boothBits = multiplierExt(2 * i + 2, 2 * i)

    encoders(i).io.y := boothBits

    val ppRaw = Wire(UInt(rawWidth.W))

    when(encoders(i).io.zero) {
      ppRaw := 0.U
    }.elsewhen(encoders(i).io.one) {
      ppRaw := Mux(encoders(i).io.neg, twosComplement(multiplicandX1), multiplicandX1)
    }.elsewhen(encoders(i).io.two) {
      ppRaw := Mux(encoders(i).io.neg, twosComplement(multiplicandX2), multiplicandX2)
    }.otherwise {
      ppRaw := 0.U
    }

    val ppExtended = signExtend(ppRaw, productWidth)
    val ppShifted  = ppExtended << (2 * i)

    io.partialProducts(i) := ppShifted(productWidth - 1, 0)
  }
}
