package special.sigma

import org.bouncycastle.crypto.ec.CustomNamedCurves
import special.collection.Col

class MockProveDlog(var isValid: Boolean, val propBytes: Col[Byte]) extends DefaultSigma {
  val curve = CustomNamedCurves.getByName("curve25519")
  def value = curve.getG
  def setValid(v: Boolean) = { isValid = v }
}

