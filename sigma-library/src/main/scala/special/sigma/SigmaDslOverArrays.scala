package special.sigma {
  import scalan.OverloadHack.Overloaded1
  import scalan._

  trait SigmaDslOverArrays extends Base { self: SigmaLibrary =>
    import TestSigmaDslBuilder._;
    import TrivialSigma._;
    import Sigma._;
    import SigmaContract._;
    import Col._;
    import WOption._;
    import WArray._;
    import WECPoint._;
    import WSpecialPredef._;
    import MonoidBuilderInst._;
    import Box._;
    import AnyValue._;
    import Context._;
    import ColOverArrayBuilder._;
    import SigmaDslBuilder._;
    import DefaultSigma._;
    import ColOverArray._;
    import ProveDlog._;
    import TestValue._;
    trait DefaultSigma extends Sigma {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      @OverloadId(value = "and_sigma") def &&(other: Rep[Sigma]): Rep[TrivialSigma] = RTrivialSigma(DefaultSigma.this.isValid.&&(other.isValid));
      @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[TrivialSigma] = RTrivialSigma(DefaultSigma.this.isValid.&&(other));
      @OverloadId(value = "or_sigma") def ||(other: Rep[Sigma]): Rep[TrivialSigma] = RTrivialSigma(DefaultSigma.this.isValid.||(other.isValid));
      @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[TrivialSigma] = RTrivialSigma(DefaultSigma.this.isValid.||(other))
    };
    trait DefaultContract extends SigmaContract {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      def verify(cond: Rep[Boolean]): Rep[Boolean] = cond;
      def verifyZK(proof: Rep[Sigma]): Rep[Boolean] = proof.isValid;
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = conditions.forall(fun(((c: Rep[Boolean]) => c)));
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = conditions.exists(fun(((c: Rep[Boolean]) => c)));
      def allZK(proofs: Rep[Col[Sigma]]): Rep[TrivialSigma] = RTrivialSigma(proofs.forall(fun(((p: Rep[Sigma]) => p.isValid))));
      def anyZK(proofs: Rep[Col[Sigma]]): Rep[TrivialSigma] = RTrivialSigma(proofs.forall(fun(((p: Rep[Sigma]) => p.isValid))))
    };
    abstract class TestBox(val idBytes: Rep[WArray[Byte]], val value: Rep[Long], val propositionBytes: Rep[Col[Byte]], val registers: Rep[Col[AnyValue]]) extends Box {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      def id: Rep[Col[Byte]] = TestBox.this.builder.Cols.fromArray[Byte](TestBox.this.idBytes);
      def getReg[T:Elem](i: Rep[Int]): Rep[WOption[T]] = RWSpecialPredef.cast[TestValue[T]](TestBox.this.registers.apply(i)).map[T](fun(((x: Rep[TestValue[T]]) => x.value)).asRep[scala.Function1[TestValue[T], T]]);
      def cost: Rep[Int] = TestBox.this.idBytes.length.+(TestBox.this.propositionBytes.length).+(TestBox.this.registers.map[Int](fun(((x$1: Rep[AnyValue]) => x$1.cost))).sum(RMonoidBuilderInst().intPlusMonoid))
    };
    abstract class TestValue[T](val value: Rep[T]) extends AnyValue {
      def cost: Rep[Int] = SigmaPredef.cost(TestValue.this.value)
    };
    abstract class TestContext(val inputs: Rep[WArray[Box]], val outputs: Rep[WArray[Box]], val height: Rep[Long], val selfBox: Rep[Box], val vars: Rep[WArray[AnyValue]]) extends Context {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      def HEIGHT: Rep[Long] = TestContext.this.height;
      def SELF: Rep[Box] = TestContext.this.selfBox;
      def INPUTS: Rep[Col[Box]] = TestContext.this.builder.Cols.fromArray[Box](TestContext.this.outputs);
      def OUTPUTS: Rep[Col[Box]] = TestContext.this.builder.Cols.fromArray[Box](TestContext.this.outputs);
      def getVar[T:Elem](id: Rep[Byte]): Rep[T] = RWSpecialPredef.cast[T](TestContext.this.vars.apply(id.toInt)).get
    };
    abstract class TestSigmaDslBuilder extends SigmaDslBuilder {
      def Cols: Rep[ColOverArrayBuilder] = RColOverArrayBuilder()
    };
    abstract class TrivialSigma(val isValid: Rep[Boolean]) extends Sigma with DefaultSigma {
      def propBytes: Rep[Col[Byte]] = TrivialSigma.this.builder.Cols.apply[Byte](IF(TrivialSigma.this.isValid).THEN(toRep(1.asInstanceOf[Byte])).ELSE(toRep(0.asInstanceOf[Byte])))
    };
    abstract class ProveDlogEvidence(val value: Rep[WECPoint]) extends ProveDlog with DefaultSigma {
      def propBytes: Rep[Col[Byte]] = RColOverArray(ProveDlogEvidence.this.value.getEncoded(toRep(true.asInstanceOf[Boolean])));
      def isValid: Rep[Boolean] = delayInvoke
    };
    trait DefaultSigmaCompanion;
    trait DefaultContractCompanion;
    trait TestBoxCompanion;
    trait TestValueCompanion;
    trait TestContextCompanion;
    trait TestSigmaDslBuilderCompanion;
    trait TrivialSigmaCompanion;
    trait ProveDlogEvidenceCompanion
  }
}