package special.sigma

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import special.collection.{ColBuilder, Col}

import scalan.{NeverInline, Reified, OverloadId}

trait DslBuilder {}
trait DslObject {
  def builder: SigmaDslBuilder
}

@sigmalang trait Sigma extends DslObject {
  def isValid: Boolean
  def propBytes: Col[Byte]
  @OverloadId("and_sigma") def &&(other: Sigma): Sigma
  @OverloadId("and_bool")  def &&(other: Boolean): Sigma
  @OverloadId("or_sigma") def ||(other: Sigma): Sigma
  @OverloadId("or_bool")  def ||(other: Boolean): Sigma
  def lazyAnd(other: => Sigma): Sigma
  def lazyOr(other: => Sigma): Sigma
}
trait SigmaBuilder extends DslBuilder {
}

@sigmalang trait ProveDlog extends Sigma {
  def value: ECPoint
}

trait AnyValue {
  def dataSize: Long
}

@sigmalang trait Box extends DslObject {
  def id: Col[Byte]
  def value: Long
  def bytes: Col[Byte]
  def bytesWithoutRef: Col[Byte]
  def propositionBytes: Col[Byte]
  def dataSize: Long
  def registers: Col[AnyValue]
  def deserialize[@Reified T:ClassTag](i: Int): Option[T]
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

  def tokens: Col[(Col[Byte], Long)] = this.R2[Col[(Col[Byte], Long)]].get
}
trait BoxBuilder extends DslBuilder {
}

trait AvlTree extends DslObject {
  def startingDigest: Col[Byte]
  def keyLength: Int
  def valueLengthOpt: Option[Int]
  def maxNumOperations: Option[Int]
  def maxDeletes: Option[Int]
  def dataSize: Long
}
trait AvlTreeBuilder extends DslBuilder {
}

trait Context {
  def builder: SigmaDslBuilder
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def HEIGHT: Long
  def SELF: Box
  def LastBlockUtxoRootHash: AvlTree
  def getVar[T:ClassTag](id: Byte): Option[T]
  def deserialize[T:ClassTag](id: Byte): Option[T]
}

trait ContextBuilder extends DslBuilder {
}

@sigmalang trait SigmaContract {
  def builder: SigmaDslBuilder
  def Collection[T](items: T*): Col[T] = this.builder.Cols.apply[T](items:_*)
  def verifyZK(cond: => Sigma): Boolean = this.builder.verifyZK(cond)
  def atLeast(bound: Int, props: Col[Sigma]): Sigma = this.builder.atLeast(bound, props)

  def allOf(conditions: Col[Boolean]): Boolean = this.builder.allOf(conditions)
  def allZK(conditions: Col[Sigma]): Sigma = this.builder.allZK(conditions)

  def anyOf(conditions: Col[Boolean]): Boolean = this.builder.anyOf(conditions)
  def anyZK(conditions: Col[Sigma]): Sigma = this.builder.anyZK(conditions)

  def PubKey(base64String: String): Sigma = this.builder.PubKey(base64String)

  def sigmaProp(b: Boolean): Sigma = this.builder.sigmaProp(b)

  def blake2b256(bytes: Col[Byte]): Col[Byte] = this.builder.blake2b256(bytes)
  def sha256(bytes: Col[Byte]): Col[Byte] = this.builder.sha256(bytes)

  def byteArrayToBigInt(bytes: Col[Byte]): BigInteger = this.builder.byteArrayToBigInt(bytes)
  def longToByteArray(l: Long): Col[Byte] = this.builder.longToByteArray(l)

  def proveDlog(g: ECPoint): Sigma = this.builder.proveDlog(g)
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): Sigma = this.builder.proveDHTuple(g, h, u, v)

  def isMember(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Boolean = this.builder.isMember(tree, key, proof)
  def groupGenerator: ECPoint = this.builder.groupGenerator

  @clause def canOpen(ctx: Context): Boolean

  def asFunction: Context => Boolean = (ctx: Context) => this.canOpen(ctx)
}

trait SigmaContractBuilder extends DslBuilder {
}

trait SigmaDslBuilder
  extends SigmaBuilder
     with BoxBuilder
     with AvlTreeBuilder
     with ContextBuilder
     with SigmaContractBuilder {
  def Cols: ColBuilder
  def verifyZK(cond: => Sigma): Boolean

  def atLeast(bound: Int, props: Col[Sigma]): Sigma

  def allOf(conditions: Col[Boolean]): Boolean
  def allZK(conditions: Col[Sigma]): Sigma

  def anyOf(conditions: Col[Boolean]): Boolean
  def anyZK(conditions: Col[Sigma]): Sigma

  def PubKey(base64String: String): Sigma

  def sigmaProp(b: Boolean): Sigma

  def blake2b256(bytes: Col[Byte]): Col[Byte]
  def sha256(bytes: Col[Byte]): Col[Byte]

  def byteArrayToBigInt(bytes: Col[Byte]): BigInteger
  def longToByteArray(l: Long): Col[Byte]

  def proveDlog(g: ECPoint): Sigma
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): Sigma

  def isMember(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Boolean
  def groupGenerator: ECPoint
}

