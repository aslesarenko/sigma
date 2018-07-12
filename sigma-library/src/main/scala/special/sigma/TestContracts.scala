package special.sigma {
  import scalan._

  trait TestContracts extends Base { self: SigmaLibrary =>
    abstract class CrowdFundingContract(val timeout: Rep[Long], val minToRaise: Rep[Long], val backerPubKey: Rep[ProveDlog], val projectPubKey: Rep[ProveDlog]) extends CrowdFunding with DefaultContract;
    abstract class DemurrageCurrencyContract(val demurragePeriod: Rep[Long], val demurrageCost: Rep[Long], val regScript: Rep[ProveDlog]) extends DemurrageCurrency with DefaultContract;
    trait CrowdFundingContractCompanion;
    trait DemurrageCurrencyContractCompanion
  }
}