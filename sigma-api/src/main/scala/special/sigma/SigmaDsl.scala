package special.sigma

import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import special.collection.{ColBuilder, Col}

import scalan.{OverloadId, Reified}

trait DslBuilder {}

@sigmalang trait Sigma {
  def builder: SigmaDslBuilder
  def isValid: Boolean
  def propBytes: Col[Byte]
  @OverloadId("and_sigma") def &&(other: Sigma): Sigma
  @OverloadId("and_bool")  def &&(other: Boolean): Sigma
  @OverloadId("or_sigma") def ||(other: Sigma): Sigma
  @OverloadId("or_bool")  def ||(other: Boolean): Sigma
}
trait SigmaBuilder extends DslBuilder {
}

@sigmalang trait ProveDlog extends Sigma {
  def value: ECPoint
}

trait AnyValue {
  def cost: Int
}

@sigmalang trait Box {
  def builder: SigmaDslBuilder
  def id: Col[Byte]
  def value: Long
  def propositionBytes: Col[Byte]
  def cost: Int
  def registers: Col[AnyValue]
  def getReg[@Reified T:ClassTag](i: Int): Option[T]

  /** Mandatory: Monetary value, in Ergo tokens */
  def R0[@Reified T:ClassTag]: Option[T] = this.getReg[T](0)

  /** Mandatory: Guarding script */
  def R1[@Reified T:ClassTag]: Option[T] = this.getReg[T](1)

  /** Mandatory: Secondary tokens */
  def R2[@Reified T:ClassTag]: Option[T] = this.getReg[T](2)

  /** Mandatory: Reference to transaction and output id where the box was created */
  def R3[@Reified T:ClassTag]: Option[T] = this.getReg[T](3)

  // Non-mandatory registers
  def R4[@Reified T:ClassTag]: Option[T] = this.getReg[T](4)
  def R5[@Reified T:ClassTag]: Option[T] = this.getReg[T](5)
  def R6[@Reified T:ClassTag]: Option[T] = this.getReg[T](6)
  def R7[@Reified T:ClassTag]: Option[T] = this.getReg[T](7)
  def R8[@Reified T:ClassTag]: Option[T] = this.getReg[T](8)
  def R9[@Reified T:ClassTag]: Option[T] = this.getReg[T](9)
}
trait BoxBuilder extends DslBuilder {
}

trait Context {
  def builder: SigmaDslBuilder
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def HEIGHT: Long
  def SELF: Box
  def getVar[T:ClassTag](id: Byte): T
}

trait ContextBuilder extends DslBuilder {
}

@sigmalang trait SigmaContract {
  def builder: SigmaDslBuilder
  def verify(cond: Boolean): Boolean = this.builder.verify(cond)
  def verifyZK(cond: Sigma): Boolean = this.builder.verifyZK(cond)

  def allOf(conditions: Col[Boolean]): Boolean = this.builder.allOf(conditions)
  def allZK(conditions: Col[Sigma]): Sigma = this.builder.allZK(conditions)

  def anyOf(conditions: Col[Boolean]): Boolean = this.builder.anyOf(conditions)
  def anyZK(conditions: Col[Sigma]): Sigma = this.builder.anyZK(conditions)

  @clause def canOpen(ctx: Context): Boolean

  def asFunction: Context => Boolean = (ctx: Context) => this.canOpen(ctx)
}

trait SigmaContractBuilder extends DslBuilder {
}

trait SigmaDslBuilder
  extends SigmaBuilder
     with BoxBuilder
     with ContextBuilder
     with SigmaContractBuilder {
  def Cols: ColBuilder
  def verify(cond: Boolean): Boolean
  def verifyZK(cond: Sigma): Boolean

  def allOf(conditions: Col[Boolean]): Boolean
  def allZK(conditions: Col[Sigma]): Sigma

  def anyOf(conditions: Col[Boolean]): Boolean
  def anyZK(conditions: Col[Sigma]): Sigma

}

