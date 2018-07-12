package special.sigma {
  import scalan._

  trait SigmaExamples extends Base { self: SigmaLibrary =>
    trait CrowdFunding extends Def[CrowdFunding] with SigmaContract {
      def timeout: Rep[Long];
      def minToRaise: Rep[Long];
      def backerPubKey: Rep[ProveDlog];
      def projectPubKey: Rep[ProveDlog];
      @clause def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
        val c1: Rep[Boolean] = ctx.HEIGHT.>=(CrowdFunding.this.timeout).&&(CrowdFunding.this.backerPubKey.isValid);
        val c2: Rep[Boolean] = ctx.HEIGHT.<(CrowdFunding.this.timeout).&&(CrowdFunding.this.projectPubKey.isValid).&&(ctx.OUTPUTS.exists(fun(((out: Rep[Box]) => out.value.>=(CrowdFunding.this.minToRaise).&&(out.propositionBytes.==(CrowdFunding.this.projectPubKey.propBytes))))));
        this.verify(c1.||(c2))
      }
    };
    trait DemurrageCurrency extends Def[DemurrageCurrency] with SigmaContract {
      def demurragePeriod: Rep[Long];
      def demurrageCost: Rep[Long];
      def regScript: Rep[ProveDlog];
      @clause def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
        val c2: Rep[Boolean] = ctx.HEIGHT.>=(ctx.SELF.R4[Long].get.+(DemurrageCurrency.this.demurragePeriod)).&&(ctx.OUTPUTS.exists(fun(((out: Rep[Box]) => out.value.>=(ctx.SELF.value.-(DemurrageCurrency.this.demurrageCost)).&&(out.propositionBytes.==(ctx.SELF.propositionBytes))))));
        this.verifyZK(DemurrageCurrency.this.regScript.||(c2))
      }
    };
    trait CrowdFundingCompanion;
    trait DemurrageCurrencyCompanion
  }
}