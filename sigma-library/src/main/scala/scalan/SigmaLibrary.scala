package scalan

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint
import special.sigma._
import special.sigma.wrappers.WrappersSpecModule

trait SigmaLibrary extends Library
  with special.sigma.wrappers.WrappersModule
  with WrappersSpecModule
  with SigmaDslModule
  with SigmaExamplesModule
  with SigmaDslOverArraysModule
  with TestContractsModule {
  implicit lazy val EcPointElement: Elem[ECPoint] = new BaseElem(CustomNamedCurves.getByName("curve25519").getG)
}
