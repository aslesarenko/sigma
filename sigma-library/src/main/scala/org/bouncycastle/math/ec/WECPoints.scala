package org.bouncycastle.math.ec {
  import scalan._

  import impl._

  import special.sigma.wrappers.WrappersModule

  trait WECPoints extends Base { self: WrappersModule =>
    @External("ECPoint") trait WECPoint extends Def[WECPoint] {
      @External def getEncoded(x$1: Rep[Boolean]): Rep[WArray[Byte]]
    };
    trait WECPointCompanion
  }
}