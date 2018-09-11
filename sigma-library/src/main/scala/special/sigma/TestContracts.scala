package special.sigma {
  import scalan._

  trait TestContracts extends Base { self: SigmaLibrary =>
    import CrowdFunding._;
    import DefaultContract._;
    import DemurrageCurrency._;
    abstract class CrowdFundingContract(val deadline: Rep[Long], val minToRaise: Rep[Long], val backerPubKey: Rep[SigmaProp], val projectPubKey: Rep[SigmaProp]) extends CrowdFunding with DefaultContract;
    abstract class DemurrageCurrencyContract(val demurragePeriod: Rep[Long], val demurrageCost: Rep[Long], val regScript: Rep[SigmaProp]) extends DemurrageCurrency with DefaultContract;
    trait CrowdFundingContractCompanion;
    trait DemurrageCurrencyContractCompanion
  }
}