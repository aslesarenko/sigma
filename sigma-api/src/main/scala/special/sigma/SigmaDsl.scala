package special.sigma

import org.bouncycastle.math.ec.ECPoint
import scala.reflect.ClassTag
import scalan.{SpecialPredef, OverloadId}
import scalan.collection.{ColBuilder, Col}

@sigmalang trait Sigma {
  def builder: SigmaDslBuilder
  def isValid: Boolean
  def propBytes: Col[Byte]
  @OverloadId("and_sigma") def &&(other: Sigma): Sigma
  @OverloadId("and_bool")  def &&(other: Boolean): Sigma
  @OverloadId("or_sigma") def ||(other: Sigma): Sigma
  @OverloadId("or_bool")  def ||(other: Boolean): Sigma
}
trait SigmaBuilder {
}

@sigmalang trait ProveDlog extends Sigma {
  def propBytes: Col[Byte]
  def value: ECPoint
}

@sigmalang trait Box {
  def builder: BoxBuilder
  def id: Col[Byte]
  def value: Long
  def propositionBytes: Col[Byte]
  def cost: Int
  def registers: Col[Any]

  def R0[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(0))
  def R1[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(1))
  def R2[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(2))
  def R3[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(3))
  def R4[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(4))
  def R5[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(5))
  def R6[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(6))
  def R7[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(7))
  def R8[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(8))
  def R9[T:ClassTag]: Option[T] = SpecialPredef.cast[T](registers(9))
}
trait BoxBuilder {
}

trait Context {
  def builder: ContextBuilder
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def HEIGHT: Long
  def SELF: Box
  def getVar[T:ClassTag](id: Byte): T
}

trait ContextBuilder {
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

trait SigmaContractBuilder {
}

trait SigmaDslBuilder
  extends SigmaBuilder
     with BoxBuilder
     with ContextBuilder
     with SigmaContractBuilder {
  def Collections: ColBuilder
}

