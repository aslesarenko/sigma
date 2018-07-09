package scalan

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint
import special.sigma.{SigmaDslOverArraysModule, SigmaDslModule, SigmaExamplesModule}

trait SigmaLibrary extends Library
  with SigmaDslModule
  with SigmaExamplesModule
  with SigmaDslOverArraysModule {
  implicit val EcPointElement: Elem[ECPoint] = new BaseElem(CustomNamedCurves.getByName("curve25519").getG)
}
