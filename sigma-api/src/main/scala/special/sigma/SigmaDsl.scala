package special.sigma

import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import special.collection.{ColBuilder, Col}

import scalan.OverloadId

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
  def builder: BoxBuilder
  def id: Col[Byte]
  def value: Long
  def propositionBytes: Col[Byte]
  def cost: Int
  def registers: Col[AnyValue]
  def getReg[T:ClassTag](i: Int): Option[T]
  def R0[T:ClassTag]: Option[T] = this.getReg[T](0)
  def R1[T:ClassTag]: Option[T] = this.getReg[T](1)
  def R2[T:ClassTag]: Option[T] = this.getReg[T](2)
  def R3[T:ClassTag]: Option[T] = this.getReg[T](3)
  def R4[T:ClassTag]: Option[T] = this.getReg[T](4)
  def R5[T:ClassTag]: Option[T] = this.getReg[T](5)
  def R6[T:ClassTag]: Option[T] = this.getReg[T](6)
  def R7[T:ClassTag]: Option[T] = this.getReg[T](7)
  def R8[T:ClassTag]: Option[T] = this.getReg[T](8)
  def R9[T:ClassTag]: Option[T] = this.getReg[T](9)
}
trait BoxBuilder extends DslBuilder {
}

trait Context {
  def builder: ContextBuilder
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def HEIGHT: Long
  def SELF: Box
  def getVar[T:ClassTag](id: Byte): T
}

trait ContextBuilder extends DslBuilder {
}

@sigmalang trait SigmaContract {
  def builder: SigmaContractBuilder
  def verify(cond: Boolean): Boolean
  def verifyZK(cond: Sigma): Boolean

  def allOf(conditions: Col[Boolean]): Boolean
  def allZK(conditions: Col[Sigma]): Sigma

  def anyOf(conditions: Col[Boolean]): Boolean
  def anyZK(conditions: Col[Sigma]): Sigma

  @clause def canOpen(ctx: Context): Boolean
}

trait SigmaContractBuilder extends DslBuilder {
}

trait SigmaDslBuilder
  extends SigmaBuilder
     with BoxBuilder
     with ContextBuilder
     with SigmaContractBuilder {
  def Cols: ColBuilder
}

