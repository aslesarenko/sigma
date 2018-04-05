package special.smart

import scalan.collection.Col


@sigma trait ByteArray

@sigma trait Sigma {
  def isValid: Boolean
  def &&(other: Sigma): Sigma
  def &&(other: Boolean): Sigma
  def ||(other: Sigma): Sigma
  def ||(other: Boolean): Sigma
}

@sigma trait ProveDlog extends Sigma {
  def propBytes: ByteArray
}

@sigma trait Box {
  def id: ByteArray
  def value: Int
  def propositionBytes: ByteArray
  def R3[T]: Option[T]
}

@sigma trait SigmaContract extends Contract {
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def SELF: Box
  def HEIGHT: Int
  def open(box: Box): Unit
  def allOf(conditions: Boolean*): Boolean
  def anyOf(conditions: Boolean*): Boolean
  def allOf(conditions: Sigma*): Sigma
  def anyOf(conditions: Sigma*): Sigma
}


