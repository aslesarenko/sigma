package special.sigma

import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import scalan.{SpecialPredef, OverloadId}
import scalan.collection.{Col, ColOverArrayBuilder, ColOverArray, MonoidBuilderInst}

class TestBox(
  val idBytes: Array[Byte],
  val value: Long,
  val propositionBytes: Col[Byte],
  val registers: Col[AnyValue]
) extends Box {
  def builder = new TestSigmaDslBuilder
  def id = builder.Cols.fromArray(idBytes)
  def getReg[T:ClassTag](i: Int): Option[T] = SpecialPredef.cast[TestValue[T]](registers(i)).map(x => x.value)
  def cost = idBytes.length + propositionBytes.length + registers.map(_.cost).sum(new MonoidBuilderInst().intPlusMonoid)
}

//class BooleanValue(val value: Boolean) extends AnyValue {
//  def cost = 1
//}
//class ByteValue(val value: Byte) extends AnyValue {
//  def cost = 1
//}
//class IntValue(val value: Int) extends AnyValue {
//  def cost = 4
//}
//class LongValue(val value: Long) extends AnyValue {
//  def cost = 4
//}
class TestValue[T](val value: T) extends AnyValue {
  def cost = SigmaPredef.cost(value)
}

class TestContext(
    val inputs: Array[Box],
    val outputs: Array[Box],
    val height: Long,
    val self: Box,
    val vars: Array[AnyValue]
) extends Context {
  def builder = new TestSigmaDslBuilder

  def HEIGHT = height
  def SELF   = self
  def INPUTS = builder.Cols.fromArray(outputs)

  def OUTPUTS = builder.Cols.fromArray(outputs)

  def getVar[T: ClassTag](id: Byte) = SpecialPredef.cast[T](vars(id)).get
}

class TestSigmaDslBuilder extends SigmaDslBuilder {
  def Cols = new ColOverArrayBuilder
}

trait DefaultSigma extends Sigma {
  def builder = new TestSigmaDslBuilder
  @OverloadId("and_sigma") def &&(other: Sigma) = new TrivialSigma(isValid && other.isValid)

  @OverloadId("and_bool")  def &&(other: Boolean) = new TrivialSigma(isValid && other)

  @OverloadId("or_sigma") def ||(other: Sigma) = new TrivialSigma(isValid || other.isValid)

  @OverloadId("or_bool")  def ||(other: Boolean) = new TrivialSigma(isValid || other)
}

class TrivialSigma(val isValid: Boolean) extends Sigma with DefaultSigma {
  def propBytes = builder.Cols(if(isValid) 1 else 0)
}

class ProveDlogEvidence(val value: ECPoint) extends ProveDlog with DefaultSigma {
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  def isValid = true
}

trait DefaultContract extends SigmaContract {
  def builder = new TestSigmaDslBuilder
  def verify(cond: Boolean) = cond

  def verifyZK(proof: Sigma) = proof.isValid

  def allOf(conditions: Col[Boolean]) = conditions.forall(c => c)
  def anyOf(conditions: Col[Boolean]) = conditions.exists(c => c)

  def allZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
  def anyZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
}


