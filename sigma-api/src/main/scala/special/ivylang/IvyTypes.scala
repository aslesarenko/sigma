package special.ivylang

import special.collection.{ColBuilder, Col}


@ivy trait Bytes extends HashableType
@ivy trait PublicKey extends HashableType
@ivy trait Signature extends HashableType

@ivy trait Time
@ivy trait Duration
@ivy trait Number
@ivy trait Value

@ivy trait HashableType
@ivy trait Sha256[T <: HashableType] extends HashableType
@ivy trait Sha1[T <: HashableType] extends HashableType
@ivy trait Ripemd160[T <: HashableType] extends HashableType

@ivy trait IvyContext {
  def checkSig(publicKey: PublicKey, sig: Signature): Boolean
  def checkMultiSig(publicKeys: Col[PublicKey], sigs: Col[Signature]): Boolean
  def after(time: Time): Boolean
  def older(duration: Duration): Boolean
  def sha256[T <: HashableType](preimage: T): Sha256[T]
  def sha1[T <: HashableType](preimage: T): Sha1[T]
  def ripemd160[T <: HashableType](preimage: T): Ripemd160[T]
  def bytes[T](item: T): Bytes
  def size(bytestring: Bytes): Number
}

@ivy trait Contract extends IvyContext {
  def Collection: ColBuilder
  def verify(cond: => Boolean): Unit
  def unlock(v: Value): Unit
}


