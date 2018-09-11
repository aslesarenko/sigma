package special.sigma

import special.wrappers.WrappersTests
import scala.language.reflectiveCalls
import scalan.SigmaLibrary

class SigmaDslTests extends WrappersTests with ContractsTestkit {
  class Ctx extends WrappersCtx with SigmaLibrary {
    import TestSigmaDslBuilder._
    val sigmaDslBuilder = RTestSigmaDslBuilder()
  }

  test("invokeUnlifted") {
    val cake = new Ctx
    import cake._
    import Liftables._
    import Context._
    import Box._
    import SigmaProp._
    import SigmaDslBuilder._
    type RSigmaDslBuilder = cake.SigmaDslBuilder
    type RContext = cake.Context
    type RBox = cake.Box
    type RSigmaProp = cake.SigmaProp
    val boxA1 = newAliceBox(1, 100, Map(1 -> 20))
    val boxA2 = newAliceBox(2, 200)
    val ctx: SContext = newContext(10, boxA1)
      .withInputs(boxA2)
      .withVariables(Map(1 -> 30, 2 -> 40))
    val p1: SSigmaProp = new special.sigma.TrivialSigma(true)
    val p2: SSigmaProp = new special.sigma.TrivialSigma(false)

    val dsl: SSigmaDslBuilder = SigmaDsl

    check(dsl, (env: DataEnv, dsl: Rep[RSigmaDslBuilder]) => dsl.sigmaProp(env.lifted(true)), dsl.sigmaProp(true))

    check(ctx, (env: DataEnv, obj: Rep[RContext]) => obj.SELF, ctx.SELF)
    check(ctx, (env: DataEnv, obj: Rep[RContext]) => obj.getVar[Int](env.lifted(1.toByte)), ctx.getVar[Int](1))

    check(boxA1, (env: DataEnv, obj: Rep[RBox]) => obj.value, boxA1.value)
    check(boxA1, (env: DataEnv, obj: Rep[RBox]) => obj.getReg[Int](env.lifted(1)), boxA1.getReg[Int](1))
    check(boxA1, (env: DataEnv, obj: Rep[RBox]) => obj.registers, boxA1.registers)

    check(p1, (env: DataEnv, p1: Rep[RSigmaProp]) => p1 && env.lifted(true), p1 && true)
    check(p1, (env: DataEnv, p1: Rep[RSigmaProp]) => p1 && env.lifted(p2), p1 && p2)

    val th = () => p2
    check(p1, (env: DataEnv, p1: Rep[RSigmaProp]) => p1.lazyAnd(env.lifted(th)), p1.lazyAnd(th()))
  }
}
