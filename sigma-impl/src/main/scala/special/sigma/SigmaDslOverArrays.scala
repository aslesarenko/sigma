package special.sigma

import scala.collection.mutable
import scalan.OverloadId
import scalan.collection.{Col, ColOverArrayBuilder, ColOverArray}

class ContextOverArrays(val inputs: Array[Box], val outputs: Array[Box], val HEIGHT: Long) extends Context {
  def builder = new ContextOverArrayBuilder

  def INPUTS = builder.Collections.fromArray(outputs)

  def OUTPUTS = builder.Collections.fromArray(outputs)
}

class ContextOverArrayBuilder extends ContextBuilder {
  def Collections = new ColOverArrayBuilder
}

trait DefaultSigma extends Sigma {
  @OverloadId("and_sigma") def &&(other: Sigma) = new TrivialSigma(isValid && other.isValid)

  @OverloadId("and_bool")  def &&(other: Boolean) = new TrivialSigma(isValid && other)

  @OverloadId("or_sigma") def ||(other: Sigma) = new TrivialSigma(isValid || other.isValid)

  @OverloadId("or_bool")  def ||(other: Boolean) = new TrivialSigma(isValid || other)
}

class TrivialSigma(val isValid: Boolean) extends Sigma with DefaultSigma

class ProveDlogEvidence(val id: Int, val isValid: Boolean) extends ProveDlog with DefaultSigma {
  def propBytes: Col[Byte] = new ColOverArray(Array.fill(id)(1))
}

trait DefaultContract extends SigmaContract {
  def verify(cond: Boolean) = cond

  def verifyZK(proof: Sigma) = proof.isValid

  def allOf(conditions: Col[Boolean]) = conditions.forall(c => c)
  def anyOf(conditions: Col[Boolean]) = conditions.exists(c => c)

  def allZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
  def anyZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
}

class CrowdFundingContract(
    val timeout: Int, val minToRaise: Int,
    val backerPubKey: ProveDlog,
    val projectPubKey: ProveDlog
) extends CrowdFunding with DefaultContract
{

}
