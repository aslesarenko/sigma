package special.sigma

import org.bouncycastle.math.ec.ECPoint

import scala.reflect.ClassTag
import scalan.{SpecialPredef, OverloadId}
import scalan.collection.{Col, ColOverArrayBuilder, ColOverArray}

class TestBox(
  val idBytes: Array[Byte],
  val value: Long,
  val propositionBytes: Col[Byte],
  val registers: Col[Any]
) extends Box {
  def builder = new SigmaDslBuilderOverArray
  def id = builder.Collections.fromArray(idBytes)
  def cost = ???
}

class ContextOverArrays(
    val inputs: Array[Box],
    val outputs: Array[Box],
    val HEIGHT: Long,
    val SELF: Box,
    val vars: Array[Any]
) extends Context {
  def builder = new SigmaDslBuilderOverArray

  def INPUTS = builder.Collections.fromArray(outputs)

  def OUTPUTS = builder.Collections.fromArray(outputs)

  def getVar[T: ClassTag](id: Byte) = SpecialPredef.cast[T](vars(id)).get
}

class SigmaDslBuilderOverArray extends SigmaDslBuilder {
  def Collections = new ColOverArrayBuilder
}

trait DefaultSigma extends Sigma {
  def builder = new SigmaDslBuilderOverArray
  @OverloadId("and_sigma") def &&(other: Sigma) = new TrivialSigma(isValid && other.isValid)

  @OverloadId("and_bool")  def &&(other: Boolean) = new TrivialSigma(isValid && other)

  @OverloadId("or_sigma") def ||(other: Sigma) = new TrivialSigma(isValid || other.isValid)

  @OverloadId("or_bool")  def ||(other: Boolean) = new TrivialSigma(isValid || other)
}

class TrivialSigma(val isValid: Boolean) extends Sigma with DefaultSigma {
  def propBytes = builder.Collections(if(isValid) 1 else 0)
}

class ProveDlogEvidence(val value: ECPoint) extends ProveDlog with DefaultSigma {
  def propBytes: Col[Byte] = new ColOverArray(value.getEncoded(true))
  def isValid = true
}

trait DefaultContract extends SigmaContract {
  def builder = new SigmaDslBuilderOverArray
  def verify(cond: Boolean) = cond

  def verifyZK(proof: Sigma) = proof.isValid

  def allOf(conditions: Col[Boolean]) = conditions.forall(c => c)
  def anyOf(conditions: Col[Boolean]) = conditions.exists(c => c)

  def allZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
  def anyZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
}


