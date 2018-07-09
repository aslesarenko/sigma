package special.sigma.wrappers

import library.WrapSpec
import org.bouncycastle.math.ec.ECPoint

class ECPointWrapSpec extends WrapSpec {
  def getEncoded[A](g: ECPoint): Array[Byte] = g.getEncoded(true)
}
