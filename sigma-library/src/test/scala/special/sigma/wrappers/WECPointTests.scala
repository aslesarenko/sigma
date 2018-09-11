package special.sigma.wrappers

import java.math.BigInteger

import org.bouncycastle.crypto.ec.CustomNamedCurves
import special.wrappers.WrappersTests
import scala.language.reflectiveCalls

class WECPointTests extends WrappersTests {
  class Ctx extends WrappersCtx with WrappersModule {
  }

  test("invokeUnlifted") {
    val ctx = new Ctx
    import ctx._
    import WECPoint._
    import WBigInteger._
    import Liftables._

    val obj = CustomNamedCurves.getByName("curve25519").getG
    val ten = BigInteger.valueOf(10L)
    check(obj, (env: DataEnv, xs: Rep[WECPoint]) => xs.add(xs), obj.add(obj))
    check(obj, (env: DataEnv, xs: Rep[WECPoint]) => xs.multiply(env.lifted(ten)), obj.multiply(ten))
    check(obj, (env: DataEnv, xs: Rep[WECPoint]) => xs.getEncoded(env.lifted(true)), obj.getEncoded(true))
  }
}
