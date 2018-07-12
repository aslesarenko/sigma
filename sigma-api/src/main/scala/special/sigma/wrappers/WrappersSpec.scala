package special.sigma.wrappers

import library.WrapSpec
import org.bouncycastle.math.ec.ECPoint
import special.sigma.SigmaPredef

import scala.reflect.ClassTag

class ECPointWrapSpec extends WrapSpec {
  def getEncoded[A](g: ECPoint): Array[Byte] = g.getEncoded(true)
}

class SigmaPredefWrapSpec extends WrapSpec {
  def cost(v: Any): Int = SigmaPredef.cost(v)
}
