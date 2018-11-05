package wrappers.org.bouncycastle.math.ec {
  import scalan._

  import impl._

  import special.sigma.wrappers.WrappersModule

  trait WECPoints extends Base { self: WrappersModule =>
    import WArray._;
    import WECPoint._;
    @External("ECPoint") @Liftable trait WECPoint extends Def[WECPoint] {
      @External def add(x$1: Rep[WECPoint]): Rep[WECPoint];
      @External def multiply(x$1: Rep[WBigInteger]): Rep[WECPoint];
      @External def getEncoded(x$1: Rep[Boolean]): Rep[WArray[Byte]]
    };
    trait WECPointCompanion
  }
}