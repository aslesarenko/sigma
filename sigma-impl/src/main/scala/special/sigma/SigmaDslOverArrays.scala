package special.sigma

import java.math.BigInteger

import com.google.common.primitives.Longs
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import special.SpecialPredef
import special.collection.{Col, ColOverArrayBuilder, ColOverArray, MonoidBuilderInst}

import scalan.{NeverInline, Internal, OverloadId}

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
  def getReg[T](i: Int)(implicit cT:ClassTag[T]): Option[T] =
    SpecialPredef.cast[TestValue[T]](registers(i)).map(x => x.value)
  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
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
  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
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
    val lastBlockUtxoRootHash: AvlTree,
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
  def LastBlockUtxoRootHash = lastBlockUtxoRootHash

  @NeverInline
  def getVar[T](id: Byte)(implicit cT: ClassTag[T]): Option[T] = SpecialPredef.cast[TestValue[T]](vars(id - 1)).map(_.value)

  @NeverInline
  def deserialize[T](id: Byte)(implicit cT: ClassTag[T]): Option[T] = ???

  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt

  @NeverInline
  def dataSize = {
    val inputsSize = INPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    val outputsSize = OUTPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    8L + SELF.dataSize + inputsSize + outputsSize + LastBlockUtxoRootHash.dataSize
  }
}

@Internal
class TestCostModel extends CostModel {
  def AccessBox: Int = CostTable.DefaultCosts("AccessBox: Context => Box")

  def GetVar: Int = CostTable.DefaultCosts("ContextVar: (Context, Byte) => Option[T]")
  def DeserializeVar: Int = CostTable.DefaultCosts("DeserializeVar: (Context, Byte) => Option[T]")

  def GetRegister: Int = CostTable.DefaultCosts("AccessRegister: (Box,Byte) => Option[T]")
  def DeserializeRegister: Int  = CostTable.DefaultCosts("DeserializeRegister: (Box,Byte) => Option[T]")

  def SelectField: Int      = CostTable.DefaultCosts("SelectField")
  def CollectionConst: Int  = CostTable.DefaultCosts("Const: () => Array[IV]")
  def AccessKiloByteOfData: Int  = CostTable.DefaultCosts("AccessKiloByteOfData")
  def dataSize[T](x: T)(implicit cT: ClassTag[T]): Long = SigmaPredef.dataSize(x)
}

class TestSigmaDslBuilder extends SigmaDslBuilder {
  @NeverInline
  def Cols = new ColOverArrayBuilder
  @NeverInline
  def Monoids = new MonoidBuilderInst
  @NeverInline
  def CostModel = new TestCostModel

  @NeverInline
  def verifyZK(proof: => SigmaProp) = proof.isValid

  @NeverInline
  def atLeast(bound: Int, props: Col[SigmaProp]): SigmaProp = {
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
  def allZK(proofs: Col[SigmaProp]) = new TrivialSigma(proofs.forall(p => p.isValid))
  @NeverInline
  def anyZK(proofs: Col[SigmaProp]) = new TrivialSigma(proofs.exists(p => p.isValid))

  @NeverInline
  def sigmaProp(b: Boolean): SigmaProp = TrivialSigma(b)

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
  def proveDlog(g: ECPoint): SigmaProp = new ProveDlogEvidence(g)

  @NeverInline
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): SigmaProp = ???

  @NeverInline
  def isMember(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Boolean = ???

  @Internal val __curve__ = CustomNamedCurves.getByName("curve25519")
  @Internal val __g__ = __curve__.getG

  @NeverInline
  def groupGenerator: ECPoint = __g__
}

trait DefaultSigma extends SigmaProp {
  def builder = new TestSigmaDslBuilder
  @NeverInline
  @OverloadId("and_sigma")
  def &&(other: SigmaProp): SigmaProp = new TrivialSigma(isValid && other.isValid)

  @NeverInline
  @OverloadId("and_bool")
  def &&(other: Boolean): SigmaProp = new TrivialSigma(isValid && other)

  @NeverInline
  @OverloadId("or_sigma")
  def ||(other: SigmaProp): SigmaProp = new TrivialSigma(isValid || other.isValid)

  @NeverInline
  @OverloadId("or_bool")
  def ||(other: Boolean): SigmaProp = new TrivialSigma(isValid || other)

  @NeverInline
  def lazyAnd(other: => SigmaProp): SigmaProp = new TrivialSigma(isValid && other.isValid)
  @NeverInline
  def lazyOr(other: => SigmaProp): SigmaProp = new TrivialSigma(isValid || other.isValid)
}

/**NOTE: this should extend SigmaProp because semantically it subclass of SigmaProp
  * and DefaultSigma is used just to mixin implementations. */
case class TrivialSigma(val isValid: Boolean) extends SigmaProp with DefaultSigma {
  @NeverInline
  def propBytes = builder.Cols(if(isValid) 1 else 0)
}

case class ProveDlogEvidence(val value: ECPoint) extends SigmaProp with DefaultSigma {
  @NeverInline
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  @NeverInline
  def isValid = true
}

case class ProveDHTEvidence(val value: ECPoint) extends SigmaProp with DefaultSigma {
  @NeverInline
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  @NeverInline
  def isValid = true
}

trait DefaultContract extends SigmaContract {
  def builder = new TestSigmaDslBuilder
}


