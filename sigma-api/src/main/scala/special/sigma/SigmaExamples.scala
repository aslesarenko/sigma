package special.sigma

abstract class CrowdFunding(
    timeout: Int, minToRaise: Int,
    backerPubKey: ProveDlog,
    projectPubKey: ProveDlog) extends SigmaContract {

  @clause def canOpen(ctx: Context, SELF: Box) = {
    val c1 = ctx.HEIGHT >= timeout && backerPubKey.isValid
    val c2 =
      ctx.HEIGHT < timeout &&
      projectPubKey.isValid &&
      ctx.OUTPUTS.exists(out => {
        out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
      })
    verify( c1 || c2 )
  }
}

abstract class DemurrageCurrency(demurragePeriod: Int, demurrageCost: Int, regScript: ProveDlog)
    extends SigmaContract {
  @clause def canOpen(ctx: Context, SELF: Box) = {
    val c2 =
      ctx.HEIGHT >= SELF.R3[Int].get + demurragePeriod &&
      ctx.OUTPUTS.exists(out => {
        out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
      })
    verifyZK ( regScript || c2 )
  }
}

