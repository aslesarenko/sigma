package special.sigma {
  import scalan.OverloadHack.Overloaded1
  import scalan._

  trait SigmaDsl extends Base { self: SigmaLibrary =>
    @sigmalang trait Sigma extends Def[Sigma] {
      def isValid: Rep[Boolean];
      @OverloadId(value = "and_sigma") def &&(other: Rep[Sigma]): Rep[Sigma];
      @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[Sigma];
      @OverloadId(value = "or_sigma") def ||(other: Rep[Sigma]): Rep[Sigma];
      @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[Sigma]
    };
    @sigmalang trait ProveDlog extends Sigma {
      def propBytes: Rep[Col[Byte]]
    };
    @sigmalang trait Box extends Def[Box] {
      def id: Rep[Col[Byte]];
      def value: Rep[Long];
      def propositionBytes: Rep[Col[Byte]];
      def R1[T]: Rep[WOption[T]];
      def R2[T]: Rep[WOption[T]];
      def R3[T]: Rep[WOption[T]];
      def R4[T]: Rep[WOption[T]];
      def R5[T]: Rep[WOption[T]];
      def R6[T]: Rep[WOption[T]];
      def R7[T]: Rep[WOption[T]];
      def R8[T]: Rep[WOption[T]];
      def R9[T]: Rep[WOption[T]]
    };
    trait Context extends Def[Context] {
      def builder: Rep[ContextBuilder];
      def OUTPUTS: Rep[Col[Box]];
      def INPUTS: Rep[Col[Box]];
      def HEIGHT: Rep[Long];
      def SELF: Rep[Box]
    };
    trait ContextBuilder extends Def[ContextBuilder] {
      def Collections: Rep[ColBuilder]
    };
    @sigmalang trait SigmaContract extends Def[SigmaContract] {
      def verify(cond: Rep[Boolean]): Rep[Boolean];
      def verifyZK(cond: Rep[Sigma]): Rep[Boolean];
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def allZK(conditions: Rep[Col[Sigma]]): Rep[Sigma];
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def anyZK(conditions: Rep[Col[Sigma]]): Rep[Sigma];
      @clause def canOpen(ctx: Rep[Context]): Rep[Boolean]
    };
    trait SigmaCompanion;
    trait ProveDlogCompanion;
    trait BoxCompanion;
    trait ContextCompanion;
    trait ContextBuilderCompanion;
    trait SigmaContractCompanion
  }
}