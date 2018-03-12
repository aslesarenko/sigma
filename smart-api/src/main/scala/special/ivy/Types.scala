package special.ivy

import special.lang
import scala.annotation.Annotation

@lang("Ivy")
class ivy extends Annotation {}

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
  def checkMultiSig(publicKeys: List[PublicKey], sigs: List[Signature]): Boolean
  def after(time: Time): Boolean
  def older(duration: Duration): Boolean
  def sha256[T <: HashableType](preimage: T): Sha256[T]
  def sha1[T <: HashableType](preimage: T): Sha1[T]
  def ripemd160[T <: HashableType](preimage: T): Ripemd160[T]
  def bytes[T](item: T): Bytes
  def size(bytestring: Bytes): Number
}

@ivy trait Contract extends IvyContext {
  def verify(cond: => Boolean): Unit
  def unlock(v: Value): Unit
}

//class ContractImpl extends Contract {
//  override def verify(cond: => Boolean): Unit = ???
//
//  override def unlock(v: Value): Unit = ???
//
//  override def checkSig(publicKey: PublicKey, sig: Signature): Boolean = ???
//
//  override def checkMultiSig(publicKeys: List[PublicKey],
//      sigs: List[Signature]): Boolean = ???
//
//  override def after(time: Time): Boolean = ???
//
//  override def older(duration: Duration): Boolean = ???
//
//  override def sha256[T <: HashableType](preimage: T): Sha256[T] = ???
//
//  override def sha1[T <: HashableType](preimage: T): Sha1[T] = ???
//
//  override def ripemd160[T <: HashableType](preimage: T): Ripemd160[T] = ???
//
//  override def bytes[T](item: T): Bytes = ???
//
//  override def size(bytestring: Bytes): Number = ???
//}
