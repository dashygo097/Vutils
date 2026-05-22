package vutils

import chisel3.Width
import chisel3.experimental.SourceInfo

package object datatype {
  implicit class IntToBinaryPointSyntax(private val value: Int) extends AnyVal {
    def BP: BinaryPoint = BinaryPoint(value)
  }

  implicit class IntToFixedPointLiteralSyntax(private val value: Int) extends AnyVal {
    def FP(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint               =
      FixedPoint.fromInt(BigInt(value), binaryPoint)
    def FP(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
      FixedPoint.fromInt(BigInt(value), width, binaryPoint)

    def FL(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
      Float.fromInt(BigInt(value), floatWidth, binaryPoint)
  }

  implicit class BigIntToFixedPointLiteralSyntax(private val value: BigInt) extends AnyVal {
    def FP(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint               =
      FixedPoint.fromInt(value, binaryPoint)
    def FP(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
      FixedPoint.fromInt(value, width, binaryPoint)

    def FL(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
      Float.fromInt(value, floatWidth, binaryPoint)
  }

  implicit class DoubleToFixedPointLiteralSyntax(private val value: Double) extends AnyVal {
    def FP(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint               =
      FixedPoint.fromDouble(value, binaryPoint)
    def FP(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
      FixedPoint.fromDouble(value, width, binaryPoint)

    def FL(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
      Float.fromDouble(value, floatWidth, binaryPoint)
  }

  implicit class BigDecimalToFixedPointLiteralSyntax(private val value: BigDecimal) extends AnyVal {
    def FP(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint               =
      FixedPoint.fromBigDecimal(value, binaryPoint)
    def FP(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
      FixedPoint.fromBigDecimal(value, width, binaryPoint)

    def FL(floatWidth: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): Float =
      Float.fromBigDecimal(value, floatWidth, binaryPoint)
  }
}
