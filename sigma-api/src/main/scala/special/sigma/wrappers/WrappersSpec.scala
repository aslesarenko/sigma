package special.sigma.wrappers

import java.math.BigInteger

import special.wrappers.WrapSpec
import org.bouncycastle.math.ec.ECPoint
import special.sigma.SigmaPredef

import scala.reflect.ClassTag

class ECPointWrapSpec extends WrapSpec {
  def getEncoded[A](g: ECPoint): Array[Byte] = g.getEncoded(true)
  def exponentiate(l: ECPoint, r: BigInteger) = l.multiply(r)
  def groupOp(l: ECPoint, r: ECPoint) = l.add(r)
}

class SigmaPredefWrapSpec extends WrapSpec {
  def dataSize(v: Any): Long = SigmaPredef.dataSize(v)
}
