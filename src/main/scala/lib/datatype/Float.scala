package vutils.datatype

import hardfloat._
import chisel3._
import chisel3.experimental.OpaqueType
import chisel3.experimental.SourceInfo
import chisel3.util.Cat
import scala.collection.immutable.SeqMap
import scala.math.BigDecimal.RoundingMode

sealed trait FloatRounding {
  private[datatype] def bits: UInt
}

object FloatRounding {
  case object RoundNearestEven extends FloatRounding {
    private[datatype] def bits: UInt = 0.U(3.W)
  }

  case object RoundTowardZero extends FloatRounding {
    private[datatype] def bits: UInt = 1.U(3.W)
  }

  case object RoundTowardNegative extends FloatRounding {
    private[datatype] def bits: UInt = 2.U(3.W)
  }

  case object RoundTowardPositive extends FloatRounding {
    private[datatype] def bits: UInt = 3.U(3.W)
  }

  case object RoundNearestMaxMag extends FloatRounding {
    private[datatype] def bits: UInt = 4.U(3.W)
  }

  case object RoundOdd extends FloatRounding {
    private[datatype] def bits: UInt = 6.U(3.W)
  }
}

sealed trait FloatTininess {
  private[datatype] def bits: Bool
}

object FloatTininess {
  case object AfterRounding extends FloatTininess {
    private[datatype] def bits: Bool = false.B
  }

  case object BeforeRounding extends FloatTininess {
    private[datatype] def bits: Bool = true.B
  }
}

object Float extends NumObject {
  def apply(floatWidth: Width, binaryPoint: BinaryPoint): Float = new Float(floatWidth, binaryPoint)

  def apply(floatWidth: Width, binaryPoint: BinaryPoint, value: Int)(implicit sourceInfo: SourceInfo): Float =
    fromInt(BigInt(value), floatWidth, binaryPoint)

  def apply(floatWidth: Width, binaryPoint: BinaryPoint, value: BigInt)(implicit sourceInfo: SourceInfo): Float =
    fromInt(value, floatWidth, binaryPoint)

  def apply(floatWidth: Width, binaryPoint: BinaryPoint, value: Double)(implicit sourceInfo: SourceInfo): Float =
    fromDouble(value, floatWidth, binaryPoint)

  def fromRaw(raw: BigInt, floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float = {
    val total = totalWidthOf(floatWidth)
    require(raw >= 0, s"Float raw literal must be non-negative, got $raw")
    require(raw < (BigInt(1) << total), s"Float raw literal $raw does not fit in $total bits")
    fromData(floatWidth, binaryPoint, raw.U(floatWidth))
  }

  def fromRawBits(floatWidth: Width, binaryPoint: BinaryPoint, raw: UInt)(implicit sourceInfo: SourceInfo): Float =
    fromData(floatWidth, binaryPoint, raw)

  def fromInt(value: BigInt, floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
    fromBigDecimal(BigDecimal(value), floatWidth, binaryPoint)

  def fromDouble(value: Double, floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
    fromRaw(doubleToRaw(value, floatWidth, binaryPoint), floatWidth, binaryPoint)

  def fromBigDecimal(
    value: BigDecimal,
    floatWidth: Width,
    binaryPoint: BinaryPoint
  )(implicit sourceInfo: SourceInfo): Float =
    fromDouble(value.toDouble, floatWidth, binaryPoint)

  def zero(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
    fromRaw(0, floatWidth, binaryPoint)

  def negativeZero(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
    fromRaw(BigInt(1) << (totalWidthOf(floatWidth) - 1), floatWidth, binaryPoint)

  def inf(
    floatWidth: Width,
    binaryPoint: BinaryPoint,
    sign: Boolean = false
  )(implicit sourceInfo: SourceInfo): Float = {
    val total   = totalWidthOf(floatWidth)
    val expW    = expWidthOf(floatWidth, binaryPoint)
    val fracW   = fractionWidthOf(binaryPoint)
    val expMax  = (BigInt(1) << expW) - 1
    val signRaw = if (sign) BigInt(1) << (total - 1) else BigInt(0)
    fromRaw(signRaw | (expMax << fracW), floatWidth, binaryPoint)
  }

  def nan(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float = {
    val expW    = expWidthOf(floatWidth, binaryPoint)
    val fracW   = fractionWidthOf(binaryPoint)
    val expMax  = (BigInt(1) << expW) - 1
    val payload = if (fracW == 0) BigInt(1) else BigInt(1) << (fracW - 1)
    fromRaw((expMax << fracW) | payload, floatWidth, binaryPoint)
  }

  def pack(
    floatWidth: Width,
    binaryPoint: BinaryPoint,
    sign: Bool,
    exponent: UInt,
    fraction: UInt
  )(implicit sourceInfo: SourceInfo): Float = {
    val expW  = expWidthOf(floatWidth, binaryPoint)
    val fracW = fractionWidthOf(binaryPoint)

    require(exponent.getWidth == expW, s"exponent width must be $expW, got ${exponent.getWidth}")
    require(fraction.getWidth == fracW, s"fraction width must be $fracW, got ${fraction.getWidth}")

    fromData(floatWidth, binaryPoint, Cat(sign, exponent, fraction))
  }

  private[datatype] def fromData(
    floatWidth: Width,
    binaryPoint: BinaryPoint,
    value: Data
  )(implicit sourceInfo: SourceInfo): Float = {
    val out = Wire(Float(floatWidth, binaryPoint))
    out.data := value.asUInt.asTypeOf(out.data)
    out
  }

  private[datatype] def totalWidthOf(floatWidth: Width): Int = {
    require(floatWidth.known, "Float requires known total width")
    require(floatWidth.get > 2, s"Float total width must be > 2, got ${floatWidth.get}")
    floatWidth.get
  }

  private[datatype] def fractionWidthOf(binaryPoint: BinaryPoint): Int =
    binaryPoint.requireKnown("Float requires known fraction width")

  private[datatype] def expWidthOf(floatWidth: Width, binaryPoint: BinaryPoint): Int = {
    val total = totalWidthOf(floatWidth)
    val fracW = fractionWidthOf(binaryPoint)
    val expW  = total - 1 - fracW

    require(fracW >= 0, s"Float fraction width must be non-negative, got $fracW")
    require(expW > 1, s"Float exponent width must be > 1, got $expW from Float($floatWidth, $binaryPoint)")

    expW
  }

  private[datatype] def hardSigWidth(binaryPoint: BinaryPoint): Int =
    fractionWidthOf(binaryPoint) + 1

  private[datatype] def toRecFN(value: Float): UInt =
    hardfloat.recFNFromFN(value.expWidth, value.hardSigWidth, value.raw)

  private[datatype] def fromRecFN(
    floatWidth: Width,
    binaryPoint: BinaryPoint,
    recoded: UInt
  )(implicit sourceInfo: SourceInfo): Float =
    fromData(
      floatWidth,
      binaryPoint,
      hardfloat.fNFromRecFN(expWidthOf(floatWidth, binaryPoint), hardSigWidth(binaryPoint), recoded)
    )

  private def doubleToRaw(value: Double, floatWidth: Width, binaryPoint: BinaryPoint): BigInt = {
    val total  = totalWidthOf(floatWidth)
    val expW   = expWidthOf(floatWidth, binaryPoint)
    val fracW  = fractionWidthOf(binaryPoint)
    val expMax = (BigInt(1) << expW) - 1
    val bias   = (BigInt(1) << (expW - 1)) - 1

    val rawDouble = java.lang.Double.doubleToRawLongBits(value)
    val sign      = (rawDouble & (1L << 63)) != 0L
    val signRaw   = if (sign) BigInt(1) << (total - 1) else BigInt(0)

    if (value.isNaN) {
      val payload = if (fracW == 0) BigInt(1) else BigInt(1) << (fracW - 1)
      return signRaw | (expMax << fracW) | payload
    }

    if (value.isInfinity) {
      return signRaw | (expMax << fracW)
    }

    if (value == 0.0) {
      return signRaw
    }

    val absValue = math.abs(value)
    val exp      = math.floor(math.log(absValue) / math.log(2.0)).toInt
    val expField = BigInt(exp) + bias

    if (expField >= expMax) {
      signRaw | (expMax << fracW)
    } else if (expField <= 0) {
      val scaled =
        BigDecimal(absValue / math.pow(2.0, 1 - bias.toInt)) * BigDecimal(2).pow(fracW)

      val frac = scaled.setScale(0, RoundingMode.HALF_UP).toBigInt

      if (frac == 0) {
        signRaw
      } else if (frac >= (BigInt(1) << fracW)) {
        signRaw | (BigInt(1) << fracW)
      } else {
        signRaw | frac
      }
    } else {
      val normalized = absValue / math.pow(2.0, exp)
      val scaled     = BigDecimal(normalized - 1.0) * BigDecimal(2).pow(fracW)

      var frac = scaled.setScale(0, RoundingMode.HALF_UP).toBigInt
      var e    = expField

      if (frac >= (BigInt(1) << fracW)) {
        frac = 0
        e += 1
      }

      if (e >= expMax) {
        signRaw | (expMax << fracW)
      } else {
        signRaw | (e << fracW) | frac
      }
    }
  }
}

sealed class Float private[datatype] (
  val floatWidth: Width,
  private val _binaryPoint: BinaryPoint
) extends Record
    with OpaqueType
    with Num[Float] {
  import FloatRounding._
  import FloatTininess._

  private[datatype] val data: UInt = UInt(floatWidth)

  override val elements: SeqMap[String, UInt] =
    SeqMap("" -> data)

  def binaryPoint: BinaryPoint = _binaryPoint

  def totalWidth: Int = Float.totalWidthOf(floatWidth)

  def fractionWidth: Int = Float.fractionWidthOf(binaryPoint)

  def expWidth: Int = Float.expWidthOf(floatWidth, binaryPoint)

  def hardSigWidth: Int = fractionWidth + 1

  def bias: BigInt = (BigInt(1) << (expWidth - 1)) - 1

  override def typeName: String = s"Float$floatWidth$binaryPoint"

  def sign: Bool = data(totalWidth - 1)

  def exponent: UInt = data(totalWidth - 2, fractionWidth)

  def fraction: UInt =
    if (fractionWidth == 0) 0.U(0.W) else data(fractionWidth - 1, 0)

  def mantissa: UInt = fraction

  def significant: UInt = fraction

  def raw: UInt = data

  def rawUInt: UInt = data

  def bits: UInt = data

  def unpack: (Bool, UInt, UInt) = (sign, exponent, fraction)

  def isCompatible(that: Float): Boolean =
    this.floatWidth == that.floatWidth && this.binaryPoint == that.binaryPoint

  def requireCompatible(that: Float): Unit =
    require(
      isCompatible(that),
      s"Float operations require matching formats: " +
        s"Float(${this.floatWidth}, ${this.binaryPoint}) vs Float(${that.floatWidth}, ${that.binaryPoint})"
    )

  def isNaN: Bool = {
    val maxExp = ((BigInt(1) << expWidth) - 1).U(expWidth.W)
    exponent === maxExp && fraction =/= 0.U
  }

  def isInf: Bool = {
    val maxExp = ((BigInt(1) << expWidth) - 1).U(expWidth.W)
    exponent === maxExp && fraction === 0.U
  }

  def isZero: Bool = exponent === 0.U && fraction === 0.U

  def isSubnormal: Bool = exponent === 0.U && fraction =/= 0.U

  def isNormal: Bool = {
    val maxExp = ((BigInt(1) << expWidth) - 1).U(expWidth.W)
    exponent =/= 0.U && exponent =/= maxExp
  }

  def isFinite: Bool =
    exponent =/= ((BigInt(1) << expWidth) - 1).U(expWidth.W)

  def isNegative: Bool = sign

  def recoded: UInt = Float.toRecFN(this)

  private def literal(value: Double)(implicit sourceInfo: SourceInfo): Float =
    Float.fromDouble(value, floatWidth, binaryPoint)

  private def binaryOp(
    that: Float,
    subOp: Bool,
    rounding: FloatRounding,
    tininess: FloatTininess
  )(implicit sourceInfo: SourceInfo): Float = {
    requireCompatible(that)

    val adder = Module(new AddRecFN(expWidth, hardSigWidth))
    adder.io.subOp          := subOp
    adder.io.a              := Float.toRecFN(this)
    adder.io.b              := Float.toRecFN(that)
    adder.io.roundingMode   := rounding.bits
    adder.io.detectTininess := tininess.bits

    Float.fromRecFN(floatWidth, binaryPoint, adder.io.out)
  }

  def add(
    that: Float,
    rounding: FloatRounding = RoundNearestEven,
    tininess: FloatTininess = AfterRounding
  )(implicit sourceInfo: SourceInfo): Float =
    binaryOp(that, false.B, rounding, tininess)

  def sub(
    that: Float,
    rounding: FloatRounding = RoundNearestEven,
    tininess: FloatTininess = AfterRounding
  )(implicit sourceInfo: SourceInfo): Float =
    binaryOp(that, true.B, rounding, tininess)

  def mul(
    that: Float,
    rounding: FloatRounding = RoundNearestEven,
    tininess: FloatTininess = AfterRounding
  )(implicit sourceInfo: SourceInfo): Float = {
    requireCompatible(that)

    val multiplier = Module(new MulRecFN(expWidth, hardSigWidth))
    multiplier.io.a              := Float.toRecFN(this)
    multiplier.io.b              := Float.toRecFN(that)
    multiplier.io.roundingMode   := rounding.bits
    multiplier.io.detectTininess := tininess.bits

    Float.fromRecFN(floatWidth, binaryPoint, multiplier.io.out)
  }

  def divUnsafeSmall(
    that: Float,
    rounding: FloatRounding = RoundNearestEven,
    tininess: FloatTininess = AfterRounding
  )(implicit sourceInfo: SourceInfo): Float = {
    requireCompatible(that)

    val divider = Module(new DivSqrtRecFN_small(expWidth, hardSigWidth, 0))
    divider.io.inValid        := true.B
    divider.io.sqrtOp         := false.B
    divider.io.a              := Float.toRecFN(this)
    divider.io.b              := Float.toRecFN(that)
    divider.io.roundingMode   := rounding.bits
    divider.io.detectTininess := tininess.bits

    Float.fromRecFN(floatWidth, binaryPoint, divider.io.out)
  }

  override def do_+(that: Float)(implicit sourceInfo: SourceInfo): Float = add(that)

  override def do_-(that: Float)(implicit sourceInfo: SourceInfo): Float = sub(that)

  override def do_*(that: Float)(implicit sourceInfo: SourceInfo): Float = mul(that)

  override def do_/(that: Float)(implicit sourceInfo: SourceInfo): Float = divUnsafeSmall(that)

  override def do_%(that: Float)(implicit sourceInfo: SourceInfo): Float =
    throw new ChiselException("Float '%' is not supported")

  override def do_abs(implicit sourceInfo: SourceInfo): Float =
    Float.fromData(floatWidth, binaryPoint, Cat(0.U(1.W), data(totalWidth - 2, 0)))

  def do_unary_-(implicit sourceInfo: SourceInfo): Float =
    Float.fromData(floatWidth, binaryPoint, Cat(~sign, data(totalWidth - 2, 0)))

  def unary_-(implicit sourceInfo: SourceInfo): Float = do_unary_-

  def +(that: Int)(implicit sourceInfo: SourceInfo): Float = this + literal(that.toDouble)

  def +(that: BigInt)(implicit sourceInfo: SourceInfo): Float = this + literal(that.toDouble)

  def +(that: Double)(implicit sourceInfo: SourceInfo): Float = this + literal(that)

  def -(that: Int)(implicit sourceInfo: SourceInfo): Float = this - literal(that.toDouble)

  def -(that: BigInt)(implicit sourceInfo: SourceInfo): Float = this - literal(that.toDouble)

  def -(that: Double)(implicit sourceInfo: SourceInfo): Float = this - literal(that)

  def *(that: Int)(implicit sourceInfo: SourceInfo): Float = this * literal(that.toDouble)

  def *(that: BigInt)(implicit sourceInfo: SourceInfo): Float = this * literal(that.toDouble)

  def *(that: Double)(implicit sourceInfo: SourceInfo): Float = this * literal(that)

  def /(that: Int)(implicit sourceInfo: SourceInfo): Float = this / literal(that.toDouble)

  def /(that: BigInt)(implicit sourceInfo: SourceInfo): Float = this / literal(that.toDouble)

  def /(that: Double)(implicit sourceInfo: SourceInfo): Float = this / literal(that)

  override def do_<(that: Float)(implicit sourceInfo: SourceInfo): Bool = {
    requireCompatible(that)

    val comparator = Module(new CompareRecFN(expWidth, hardSigWidth))
    comparator.io.a         := Float.toRecFN(this)
    comparator.io.b         := Float.toRecFN(that)
    comparator.io.signaling := false.B

    comparator.io.lt
  }

  override def do_<=(that: Float)(implicit sourceInfo: SourceInfo): Bool = {
    requireCompatible(that)

    val comparator = Module(new CompareRecFN(expWidth, hardSigWidth))
    comparator.io.a         := Float.toRecFN(this)
    comparator.io.b         := Float.toRecFN(that)
    comparator.io.signaling := false.B

    comparator.io.lt || comparator.io.eq
  }

  override def do_>(that: Float)(implicit sourceInfo: SourceInfo): Bool = {
    requireCompatible(that)

    val comparator = Module(new CompareRecFN(expWidth, hardSigWidth))
    comparator.io.a         := Float.toRecFN(this)
    comparator.io.b         := Float.toRecFN(that)
    comparator.io.signaling := false.B

    comparator.io.gt
  }

  override def do_>=(that: Float)(implicit sourceInfo: SourceInfo): Bool = {
    requireCompatible(that)

    val comparator = Module(new CompareRecFN(expWidth, hardSigWidth))
    comparator.io.a         := Float.toRecFN(this)
    comparator.io.b         := Float.toRecFN(that)
    comparator.io.signaling := false.B

    comparator.io.gt || comparator.io.eq
  }

  def ===(that: Float)(implicit sourceInfo: SourceInfo): Bool = {
    requireCompatible(that)

    val comparator = Module(new CompareRecFN(expWidth, hardSigWidth))
    comparator.io.a         := Float.toRecFN(this)
    comparator.io.b         := Float.toRecFN(that)
    comparator.io.signaling := false.B

    comparator.io.eq
  }

  def =/=(that: Float)(implicit sourceInfo: SourceInfo): Bool = !(this === that)

  def <(that: Int)(implicit sourceInfo: SourceInfo): Bool = this < literal(that.toDouble)

  def <(that: Double)(implicit sourceInfo: SourceInfo): Bool = this < literal(that)

  def <=(that: Int)(implicit sourceInfo: SourceInfo): Bool = this <= literal(that.toDouble)

  def <=(that: Double)(implicit sourceInfo: SourceInfo): Bool = this <= literal(that)

  def >(that: Int)(implicit sourceInfo: SourceInfo): Bool = this > literal(that.toDouble)

  def >(that: Double)(implicit sourceInfo: SourceInfo): Bool = this > literal(that)

  def >=(that: Int)(implicit sourceInfo: SourceInfo): Bool = this >= literal(that.toDouble)

  def >=(that: Double)(implicit sourceInfo: SourceInfo): Bool = this >= literal(that)

  def ===(that: Int)(implicit sourceInfo: SourceInfo): Bool = this === literal(that.toDouble)

  def ===(that: Double)(implicit sourceInfo: SourceInfo): Bool = this === literal(that)

  def =/=(that: Int)(implicit sourceInfo: SourceInfo): Bool = this =/= literal(that.toDouble)

  def =/=(that: Double)(implicit sourceInfo: SourceInfo): Bool = this =/= literal(that)

  def apply(idx: Int)(implicit sourceInfo: SourceInfo): Bool = {
    require(idx >= 0 && idx < totalWidth, s"Index $idx out of bounds for Float width $totalWidth")
    data(idx)
  }

  def apply(idx: UInt)(implicit sourceInfo: SourceInfo): Bool = data(idx)

  def apply(high: Int, low: Int)(implicit sourceInfo: SourceInfo): UInt = {
    require(
      high >= low && low >= 0 && high < totalWidth,
      s"Index range [$high, $low] out of bounds for Float width $totalWidth"
    )
    data(high, low)
  }

  private def connectOp(
    that: Data,
    connect: (Data, Data) => Unit
  ): Unit =
    that match {
      case that: Float =>
        requireCompatible(that)
        connect(data, that.data)

      case that @ DontCare =>
        connect(data, that)

      case _ =>
        throw new ChiselException(s"cannot connect Float and $that")
    }

  override def connect(that: Data)(implicit sourceInfo: SourceInfo): Unit =
    connectOp(that, _ := _)

  override def bulkConnect(that: Data)(implicit sourceInfo: SourceInfo): Unit =
    connectOp(that, _ <> _)

  override protected def _fromUInt(that: UInt)(implicit sourceInfo: SourceInfo): Data = {
    val out = Wire(this.cloneType)
    out.data := that.asTypeOf(out.data)
    out
  }

  override def litOption: Option[BigInt] = data.litOption

  override def litValue: BigInt = data.litValue

  override def toString: String =
    litOption match {
      case Some(value) => s"Float($floatWidth, $binaryPoint)(raw=0x${value.toString(16)})"
      case None        => s"Float($floatWidth, $binaryPoint)"
    }
}
