package special.sigma

import java.math.BigInteger

import com.google.common.primitives.Longs
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import special.SpecialPredef
import special.collection.{Col, ColOverArrayBuilder, ColOverArray}

import scalan.{NeverInline, OverloadId, Internal}

class TestBox(
  val id: Col[Byte],
  val value: Long,
  val bytes: Col[Byte],
  val bytesWithoutRef: Col[Byte],
  val propositionBytes: Col[Byte],
  val registers: Col[AnyValue]) extends Box
{
  def builder = new TestSigmaDslBuilder
  @NeverInline
  def getReg[T:ClassTag](i: Int): Option[T] =
    SpecialPredef.cast[TestValue[T]](registers(i)).map(x => x.value)
  @NeverInline
  def dataSize = bytes.length
  @NeverInline
  def deserialize[T](i: Int)(implicit cT:ClassTag[T]): Option[T] = ???
}

case class TestAvlTree(
    startingDigest: Col[Byte],
    keyLength: Int,
    valueLengthOpt: Option[Int] = None,
    maxNumOperations: Option[Int] = None,
    maxDeletes: Option[Int] = None ) extends AvlTree {
  def builder = new TestSigmaDslBuilder
  @NeverInline
  def dataSize = startingDigest.length + 4 + valueLengthOpt.fold(0L)(_ => 4)
}

class TestValue[T](val value: T) extends AnyValue {
  @NeverInline
  def dataSize = SigmaPredef.dataSize(value)
}

class TestContext(
    val inputs: Array[Box],
    val outputs: Array[Box],
    val height: Long,
    val selfBox: Box,
    val LastBlockUtxoRootHash: AvlTree,
    val vars: Array[AnyValue]
) extends Context {
  def builder = new TestSigmaDslBuilder

  @NeverInline
  def HEIGHT = height
  @NeverInline
  def SELF   = selfBox
  @NeverInline
  def INPUTS = builder.Cols.fromArray(outputs)

  @NeverInline
  def OUTPUTS = builder.Cols.fromArray(outputs)

  @NeverInline
  def getVar[T](id: Byte)(implicit cT: ClassTag[T]): Option[T] = SpecialPredef.cast[TestValue[T]](vars(id - 1)).map(_.value)

  @NeverInline
  def deserialize[T](id: Byte)(implicit cT: ClassTag[T]): Option[T] = ???
}

class TestSigmaDslBuilder extends SigmaDslBuilder {
  def Cols = new ColOverArrayBuilder

  @NeverInline
  def verifyZK(proof: => Sigma) = proof.isValid

  @NeverInline
  def atLeast(bound: Int, props: Col[Sigma]): Sigma = {
    if (bound <= 0) return TrivialSigma(true)
    if (bound > props.length) return TrivialSigma(false)
    var nValids = 0
    for (p <- props) {
      if (p.isValid)  nValids += 1
      if (nValids == bound) return TrivialSigma(true)
    }
    TrivialSigma(false)
  }

  @NeverInline
  def allOf(conditions: Col[Boolean]) = conditions.forall(c => c)
  @NeverInline
  def anyOf(conditions: Col[Boolean]) = conditions.exists(c => c)

  @NeverInline
  def allZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
  @NeverInline
  def anyZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.exists(p => p.isValid))

  @NeverInline
  def sigmaProp(b: Boolean): Sigma = TrivialSigma(b)

  @NeverInline
  def blake2b256(bytes: Col[Byte]): Col[Byte] = ???

  @NeverInline
  def sha256(bytes: Col[Byte]): Col[Byte] = ???

  @NeverInline
  def PubKey(base64String: String) = ???

  @NeverInline
  def byteArrayToBigInt(bytes: Col[Byte]): BigInteger = new BigInteger(bytes.arr)

  @NeverInline
  def longToByteArray(l: Long): Col[Byte] = Cols.fromArray(Longs.toByteArray(l))

  @NeverInline
  def proveDlog(g: ECPoint): Sigma = new ProveDlogEvidence(g)

  @NeverInline
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): Sigma = ???

  @NeverInline
  def isMember(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Boolean = ???

  @Internal val __curve__ = CustomNamedCurves.getByName("curve25519")
  @Internal val __g__ = __curve__.getG

  @NeverInline
  def groupGenerator: ECPoint = __g__
}

trait DefaultSigma extends Sigma {
  def builder = new TestSigmaDslBuilder
  @NeverInline
  @OverloadId("and_sigma")
  def &&(other: Sigma) = new TrivialSigma(isValid && other.isValid)

  @NeverInline
  @OverloadId("and_bool")
  def &&(other: Boolean) = new TrivialSigma(isValid && other)

  @NeverInline
  @OverloadId("or_sigma")
  def ||(other: Sigma) = new TrivialSigma(isValid || other.isValid)

  @NeverInline
  @OverloadId("or_bool")
  def ||(other: Boolean) = new TrivialSigma(isValid || other)

  @NeverInline
  def lazyAnd(other: => Sigma) = new TrivialSigma(isValid && other.isValid)
  @NeverInline
  def lazyOr(other: => Sigma) = new TrivialSigma(isValid || other.isValid)
}

case class TrivialSigma(val isValid: Boolean) extends Sigma with DefaultSigma {
  @NeverInline
  def propBytes = builder.Cols(if(isValid) 1 else 0)
}

case class ProveDlogEvidence(val value: ECPoint) extends ProveDlog with DefaultSigma {
  @NeverInline
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  @NeverInline
  def isValid = true
}

case class ProveDHTEvidence(val value: ECPoint) extends ProveDlog with DefaultSigma {
  @NeverInline
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  @NeverInline
  def isValid = true
}

trait DefaultContract extends SigmaContract {
  def builder = new TestSigmaDslBuilder
}


