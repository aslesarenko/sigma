package special.sigma {
  import scalan.OverloadHack.Overloaded1
  import scalan._

  trait SigmaDsl extends Base { self: SigmaLibrary =>
    trait DslBuilder extends Def[DslBuilder];
    @sigmalang trait Sigma extends Def[Sigma] {
      def builder: Rep[SigmaDslBuilder];
      def isValid: Rep[Boolean];
      def propBytes: Rep[Col[Byte]];
      @OverloadId(value = "and_sigma") def &&(other: Rep[Sigma]): Rep[Sigma];
      @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[Sigma];
      @OverloadId(value = "or_sigma") def ||(other: Rep[Sigma]): Rep[Sigma];
      @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[Sigma]
    };
    trait SigmaBuilder extends DslBuilder;
    @sigmalang trait ProveDlog extends Sigma {
      def value: Rep[WECPoint]
    };
    trait AnyValue extends Def[AnyValue] {
      def cost: Rep[Int]
    };
    @sigmalang trait Box extends Def[Box] {
      def builder: Rep[BoxBuilder];
      def id: Rep[Col[Byte]];
      def value: Rep[Long];
      def propositionBytes: Rep[Col[Byte]];
      def cost: Rep[Int];
      def registers: Rep[Col[AnyValue]];
      def getReg[T:Elem](i: Rep[Int]): Rep[WOption[T]];
      def R0[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(0.asInstanceOf[Int]));
      def R1[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(1.asInstanceOf[Int]));
      def R2[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(2.asInstanceOf[Int]));
      def R3[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(3.asInstanceOf[Int]));
      def R4[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(4.asInstanceOf[Int]));
      def R5[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(5.asInstanceOf[Int]));
      def R6[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(6.asInstanceOf[Int]));
      def R7[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(7.asInstanceOf[Int]));
      def R8[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(8.asInstanceOf[Int]));
      def R9[T:Elem]: Rep[WOption[T]] = Box.this.getReg[T](toRep(9.asInstanceOf[Int]))
    };
    trait BoxBuilder extends DslBuilder;
    trait Context extends Def[Context] {
      def builder: Rep[ContextBuilder];
      def OUTPUTS: Rep[Col[Box]];
      def INPUTS: Rep[Col[Box]];
      def HEIGHT: Rep[Long];
      def SELF: Rep[Box];
      def getVar[T:Elem](id: Rep[Byte]): Rep[T]
    };
    trait ContextBuilder extends DslBuilder;
    @sigmalang trait SigmaContract extends Def[SigmaContract] {
      def builder: Rep[SigmaContractBuilder];
      def verify(cond: Rep[Boolean]): Rep[Boolean];
      def verifyZK(cond: Rep[Sigma]): Rep[Boolean];
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def allZK(conditions: Rep[Col[Sigma]]): Rep[Sigma];
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def anyZK(conditions: Rep[Col[Sigma]]): Rep[Sigma];
      @clause def canOpen(ctx: Rep[Context]): Rep[Boolean]
    };
    trait SigmaContractBuilder extends DslBuilder;
    trait SigmaDslBuilder extends SigmaBuilder with BoxBuilder with ContextBuilder with SigmaContractBuilder {
      def Cols: Rep[ColBuilder]
    };
    trait DslBuilderCompanion;
    trait SigmaCompanion;
    trait SigmaBuilderCompanion;
    trait ProveDlogCompanion;
    trait AnyValueCompanion;
    trait BoxCompanion;
    trait BoxBuilderCompanion;
    trait ContextCompanion;
    trait ContextBuilderCompanion;
    trait SigmaContractCompanion;
    trait SigmaContractBuilderCompanion;
    trait SigmaDslBuilderCompanion
  }
}