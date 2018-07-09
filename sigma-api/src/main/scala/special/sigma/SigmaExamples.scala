package special.sigma

trait CrowdFunding extends SigmaContract {
  def timeout: Long
  def minToRaise: Long
  def backerPubKey: ProveDlog
  def projectPubKey: ProveDlog

  @clause def canOpen(ctx: Context) = {
    val c1 = ctx.HEIGHT >= timeout && backerPubKey.isValid
    val c2 =
      ctx.HEIGHT < timeout &&
      projectPubKey.isValid &&
      ctx.OUTPUTS.exists(out => {
        out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
      })
    this.verify( c1 || c2 )
  }
}

trait DemurrageCurrency extends SigmaContract {
  def demurragePeriod: Long
  def demurrageCost: Long
  def regScript: ProveDlog

  @clause def canOpen(ctx: Context) = {
    val c2 =
      ctx.HEIGHT >= ctx.SELF.R4[Long].get + demurragePeriod &&
      ctx.OUTPUTS.exists(out => {
        out.value >= ctx.SELF.value - demurrageCost && out.propositionBytes == ctx.SELF.propositionBytes
      })
    this.verifyZK ( regScript || c2 )
  }
}

