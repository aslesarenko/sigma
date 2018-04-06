package special.sigma

abstract class CrowdFunding(
    timeout: Int, minToRaise: Int,
    backerPubKey: ProveDlog,
    projectPubKey: ProveDlog) extends SigmaContract {
  @clause def spend() {
    val c1 = HEIGHT >= timeout && backerPubKey.isValid
    val c2 = allOf(
      HEIGHT < timeout,
      projectPubKey.isValid,
      OUTPUTS.exists(out => {
        out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
      })
    )
    verify { c1 || c2 }
    open(SELF)
  }
}

abstract class DemurrageCurrency(demurragePeriod: Int, demurrageCost: Int, regScript: ProveDlog)
    extends SigmaContract {
  @clause def spend() {
    val c2 = allOf(
      HEIGHT >= SELF.R3[Int].get + demurragePeriod,
      OUTPUTS.exists(out => {
        out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
      })
    )
    verifyZK { regScript || c2 }
    open(SELF)
  }
}

