package special.sigma

trait CrowdFunding extends SigmaContract {
  def timeout: Int
  def minToRaise: Int
  def backerPubKey: ProveDlog
  def projectPubKey: ProveDlog

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

trait DemurrageCurrency extends SigmaContract {
  def demurragePeriod: Int
  def demurrageCost: Int
  def regScript: ProveDlog

  @clause def canOpen(ctx: Context, SELF: Box) = {
    val c2 =
      ctx.HEIGHT >= SELF.R3[Int].get + demurragePeriod &&
      ctx.OUTPUTS.exists(out => {
        out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
      })
    verifyZK ( regScript || c2 )
  }
}

