package special.sigma {
  import scalan._

  trait SigmaExamples extends Base { self: SigmaLibrary =>
    trait CrowdFunding extends Def[CrowdFunding] with SigmaContract {
      def timeout: Rep[Int];
      def minToRaise: Rep[Int];
      def backerPubKey: Rep[ProveDlog];
      def projectPubKey: Rep[ProveDlog];
      @clause def canOpen(ctx: Rep[Context], SELF: Rep[Box]): Rep[Boolean] = {
        val c1: Rep[Boolean] = ctx.HEIGHT.>=(CrowdFunding.this.timeout.toLong).&&(CrowdFunding.this.backerPubKey.isValid);
        val c2: Rep[Boolean] = ctx.HEIGHT.<(CrowdFunding.this.timeout.toLong).&&(CrowdFunding.this.projectPubKey.isValid).&&(ctx.OUTPUTS.exists(fun(((out: Rep[Box]) => out.value.>=(CrowdFunding.this.minToRaise.toLong).&&(out.propositionBytes.==(CrowdFunding.this.projectPubKey.propBytes))))));
        CrowdFunding.this.verify(c1.||(c2))
      }
    };
    trait DemurrageCurrency extends Def[DemurrageCurrency] with SigmaContract {
      def demurragePeriod: Rep[Int];
      def demurrageCost: Rep[Int];
      def regScript: Rep[ProveDlog];
      @clause def canOpen(ctx: Rep[Context], SELF: Rep[Box]): Rep[Boolean] = {
        val c2: Rep[Boolean] = ctx.HEIGHT.>=(SELF.R3[Int].get.toLong.+(DemurrageCurrency.this.demurragePeriod.toLong)).&&(ctx.OUTPUTS.exists(fun(((out: Rep[Box]) => out.value.>=(SELF.value.-(DemurrageCurrency.this.demurrageCost.toLong)).&&(out.propositionBytes.==(SELF.propositionBytes))))));
        DemurrageCurrency.this.verifyZK(DemurrageCurrency.this.regScript.||(c2))
      }
    };
    trait CrowdFundingCompanion;
    trait DemurrageCurrencyCompanion
  }
}