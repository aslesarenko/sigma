package special.sigma {
  import scalan._

  import impl._

  import special.sigma.wrappers.WrappersModule

  trait WSigmaPredefs extends Base { self: WrappersModule =>
    @External("SigmaPredef") trait WSigmaPredef extends Def[WSigmaPredef];
    trait WSigmaPredefCompanion {
      @External def cost(v: Rep[Any]): Rep[Int]
    }
  }
}