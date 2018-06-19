package special.sigma

import scalan.collection.{Col, ColBuilder}

@sigmalang trait Sigma {
  def isValid: Boolean
  def &&(other: Sigma): Sigma
  def &&(other: Boolean): Sigma
  def ||(other: Sigma): Sigma
  def ||(other: Boolean): Sigma
}

@sigmalang trait ProveDlog extends Sigma {
  def propBytes: Col[Byte]
}

@sigmalang trait Box {
  def id: Col[Byte]
  def value: Long
  def propositionBytes: Col[Byte]
  def R1[T]: Option[T]
  def R2[T]: Option[T]
  def R3[T]: Option[T]
  def R4[T]: Option[T]
  def R5[T]: Option[T]
  def R6[T]: Option[T]
  def R7[T]: Option[T]
  def R8[T]: Option[T]
  def R9[T]: Option[T]
}

trait Context {
  def builder: ContextBuilder
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def HEIGHT: Long
}

trait ContextBuilder {
  def Collections: ColBuilder
}

@sigmalang trait SigmaContract {
  def verify(cond: => Boolean): Boolean
  def verifyZK(cond: => Sigma): Boolean

  def allOf(conditions: Col[Boolean]): Boolean
  def allZK(conditions: Col[Sigma]): Sigma

  def anyOf(conditions: Col[Boolean]): Boolean
  def anyZK(conditions: Col[Sigma]): Sigma

  @clause def canOpen(ctx: Context, SELF: Box): Boolean
}


