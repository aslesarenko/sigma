package special.sigma

import java.math.BigInteger

import com.google.common.primitives.Longs
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import special.SpecialPredef
import special.collection.{Col, ColOverArrayBuilder, ColOverArray}

import scalan.{NeverInline, OverloadId}

class TestBox(
  val id: Col[Byte],
  val value: Long,
  val bytes: Col[Byte],
  val bytesWithoutRef: Col[Byte],
  val propositionBytes: Col[Byte],
  val registers: Col[AnyValue]) extends Box
{
  def builder = new TestSigmaDslBuilder
  def getReg[T:ClassTag](i: Int): Option[T] =
    SpecialPredef.cast[TestValue[T]](registers(i)).map(x => x.value)
  def dataSize = bytes.length
  @NeverInline
  def deserialize[T: ClassTag](i: Int): Option[T] = ???
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

  def HEIGHT = height
  def SELF   = selfBox
  def INPUTS = builder.Cols.fromArray(outputs)

  def OUTPUTS = builder.Cols.fromArray(outputs)

  def getVar[T: ClassTag](id: Byte): Option[T] = SpecialPredef.cast[TestValue[T]](vars(id - 1)).map(_.value)

  @NeverInline
  def deserialize[T: ClassTag](id: Byte): Option[T] = ???
}

class TestSigmaDslBuilder extends SigmaDslBuilder {
  def Cols = new ColOverArrayBuilder

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

  def allOf(conditions: Col[Boolean]) = conditions.forall(c => c)
  def anyOf(conditions: Col[Boolean]) = conditions.exists(c => c)

  def allZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
  def anyZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.exists(p => p.isValid))

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

  val curve = CustomNamedCurves.getByName("curve25519")
  val g = curve.getG
  def groupGenerator: ECPoint = g
}

trait DefaultSigma extends Sigma {
  def builder = new TestSigmaDslBuilder
  @OverloadId("and_sigma") def &&(other: Sigma) = new TrivialSigma(isValid && other.isValid)

  @OverloadId("and_bool")  def &&(other: Boolean) = new TrivialSigma(isValid && other)

  @OverloadId("or_sigma") def ||(other: Sigma) = new TrivialSigma(isValid || other.isValid)

  @OverloadId("or_bool")  def ||(other: Boolean) = new TrivialSigma(isValid || other)

  def lazyAnd(other: => Sigma) = new TrivialSigma(isValid && other.isValid)
  def lazyOr(other: => Sigma) = new TrivialSigma(isValid || other.isValid)
}

case class TrivialSigma(val isValid: Boolean) extends Sigma with DefaultSigma {
  def propBytes = builder.Cols(if(isValid) 1 else 0)
}

case class ProveDlogEvidence(val value: ECPoint) extends ProveDlog with DefaultSigma {
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  def isValid = true
}

case class ProveDHTEvidence(val value: ECPoint) extends ProveDlog with DefaultSigma {
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  def isValid = true
}

trait DefaultContract extends SigmaContract {
  def builder = new TestSigmaDslBuilder
}


