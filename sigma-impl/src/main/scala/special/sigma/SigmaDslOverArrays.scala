package special.sigma

import scala.collection.mutable
import scalan.collection.{Col, ColOverArrayBuilder, ColOverArray}

class ContextOverArrays(val inputs: Array[Box], val outputs: Array[Box], val HEIGHT: Long) extends Context {
  def builder = new ContextOverArrayBuilder

  def INPUTS = builder.Collections.fromArray(outputs)

  def OUTPUTS = builder.Collections.fromArray(outputs)
}

class ContextOverArrayBuilder extends ContextBuilder {
  def Collections = new ColOverArrayBuilder
}

class TrivialSigma(val isValid: Boolean) extends Sigma with DefaultSigma

trait DefaultSigma extends Sigma {
  def &&(other: Sigma) = new TrivialSigma(isValid && other.isValid)

  def &&(other: Boolean) = new TrivialSigma(isValid && other)

  def ||(other: Sigma) = new TrivialSigma(isValid || other.isValid)

  def ||(other: Boolean) = new TrivialSigma(isValid || other)
}

class ProveDlogEvidence(val id: Int, val isValid: Boolean) extends ProveDlog with DefaultSigma {
  def propBytes: Col[Byte] = new ColOverArray(Array.fill(id)(1))
}

trait DefaultContract extends SigmaContract {
  def verify(cond: => Boolean) = cond

  def verifyZK(proof: =>Sigma) = proof.isValid

  def allOf(conditions: Col[Boolean]) = conditions.forall(c => c)
  def anyOf(conditions: Col[Boolean]) = conditions.exists(c => c)

  def allZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
  def anyZK(proofs: Col[Sigma]) = new TrivialSigma(proofs.forall(p => p.isValid))
}

class CrowdFundingContract(
    timeout: Int, minToRaise: Int,
    backerPubKey: ProveDlog,
    projectPubKey: ProveDlog
) extends CrowdFunding(timeout, minToRaise, backerPubKey, projectPubKey)
     with DefaultContract
{

}
