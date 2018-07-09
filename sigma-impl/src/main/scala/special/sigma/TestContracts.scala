package special.sigma

class CrowdFundingContract(
    val timeout: Long, val minToRaise: Long,
    val backerPubKey: ProveDlog,
    val projectPubKey: ProveDlog
) extends CrowdFunding with DefaultContract {
}

class DemurrageCurrencyContract(
    val demurragePeriod: Long,
    val demurrageCost: Long,
    val regScript: ProveDlog
) extends DemurrageCurrency with DefaultContract {
}

