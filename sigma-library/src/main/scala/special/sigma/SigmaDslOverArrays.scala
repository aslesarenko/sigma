package special.sigma {
  import scalan.OverloadHack.Overloaded1
  import scalan._

  trait SigmaDslOverArrays extends Base { self: SigmaLibrary =>
    trait DefaultSigma extends Sigma {
      @OverloadId(value = "and_sigma") def &&(other: Rep[Sigma]): Rep[TrivialSigma] = TrivialSigma(DefaultSigma.this.isValid.&&(other.isValid));
      @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[TrivialSigma] = TrivialSigma(DefaultSigma.this.isValid.&&(other));
      @OverloadId(value = "or_sigma") def ||(other: Rep[Sigma]): Rep[TrivialSigma] = TrivialSigma(DefaultSigma.this.isValid.||(other.isValid));
      @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[TrivialSigma] = TrivialSigma(DefaultSigma.this.isValid.||(other))
    };
    trait DefaultContract extends SigmaContract {
      def verify(cond: Rep[Boolean]): Rep[Boolean] = cond;
      def verifyZK(proof: Rep[Sigma]): Rep[Boolean] = proof.isValid;
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = conditions.forall(fun(((c: Rep[Boolean]) => c)));
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = conditions.exists(fun(((c: Rep[Boolean]) => c)));
      def allZK(proofs: Rep[Col[Sigma]]): Rep[TrivialSigma] = TrivialSigma(proofs.forall(fun(((p: Rep[Sigma]) => p.isValid))));
      def anyZK(proofs: Rep[Col[Sigma]]): Rep[TrivialSigma] = TrivialSigma(proofs.forall(fun(((p: Rep[Sigma]) => p.isValid))))
    };
    abstract class ContextOverArrays(val inputs: Rep[WArray[Box]], val outputs: Rep[WArray[Box]], val HEIGHT: Rep[Long], val SELF: Rep[Box]) extends Context {
      def builder: Rep[ContextOverArrayBuilder] = ContextOverArrayBuilder();
      def INPUTS: Rep[Col[Box]] = ContextOverArrays.this.builder.Collections.fromArray[Box](ContextOverArrays.this.outputs);
      def OUTPUTS: Rep[Col[Box]] = ContextOverArrays.this.builder.Collections.fromArray[Box](ContextOverArrays.this.outputs)
    };
    abstract class ContextOverArrayBuilder extends ContextBuilder {
      def Collections: Rep[ColOverArrayBuilder] = ColOverArrayBuilder()
    };
    abstract class TrivialSigma(val isValid: Rep[Boolean]) extends Sigma with DefaultSigma;
    abstract class ProveDlogEvidence(val id: Rep[Int], val isValid: Rep[Boolean]) extends ProveDlog with DefaultSigma {
      def propBytes: Rep[Col[Byte]] = ColOverArray(WArray.fill[Byte](ProveDlogEvidence.this.id, Thunk(toRep(1.asInstanceOf[Byte]))))
    };
    abstract class CrowdFundingContract(val timeout: Rep[Long], val minToRaise: Rep[Long], val backerPubKey: Rep[ProveDlog], val projectPubKey: Rep[ProveDlog]) extends CrowdFunding with DefaultContract;
    trait DefaultSigmaCompanion;
    trait DefaultContractCompanion;
    trait ContextOverArraysCompanion;
    trait ContextOverArrayBuilderCompanion;
    trait TrivialSigmaCompanion;
    trait ProveDlogEvidenceCompanion;
    trait CrowdFundingContractCompanion
  }
}