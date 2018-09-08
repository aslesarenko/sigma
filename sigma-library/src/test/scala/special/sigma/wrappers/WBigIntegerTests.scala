package special.sigma.wrappers

import java.math.BigInteger
import special.wrappers.WrappersTests
import scala.collection.mutable
import scala.language.reflectiveCalls

class WBigIntegerTests extends WrappersTests {
  class Ctx extends WrappersCtx with WrappersModule {
  }

  test("invokeUnlifted") {
    val ctx = new Ctx
    import ctx._
    import Liftables._
    import WBigInteger._

    val obj = BigInteger.valueOf(10L)

    check(obj, (env: DataEnv, xs: Rep[WBigInteger]) => xs.add(env.lifted(BigInteger.ONE)), obj.add(BigInteger.ONE))
    check(obj, (env: DataEnv, xs: Rep[WBigInteger]) => xs.multiply(xs), obj.multiply(obj))
  }
}
