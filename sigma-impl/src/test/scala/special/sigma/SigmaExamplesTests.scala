package special.sigma

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.scalatest.FunSuite

import scalan.collection.{ColOverArrayBuilder, Col}

trait ContractsTestSuite {
  val R0 = 0.toByte;
  val R1 = 1.toByte;
  val R2 = 2.toByte;
  val R3 = 3.toByte;
  val R4 = 4.toByte;
  val R5 = 5.toByte;
  val R6 = 6.toByte;
  val R7 = 7.toByte;
  val R8 = 8.toByte;
  val R9 = 9.toByte;

  val curve = CustomNamedCurves.getByName("curve25519")
  val g = curve.getG
  val Cols = new ColOverArrayBuilder
  val noRegisters = Cols.fromArray(Array[AnyValue]())
  val noBytes = Cols.fromArray(Array[Byte]())
  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()

  def regs(m: Map[Byte, Any]): Col[AnyValue] = {
    val res = new Array[AnyValue](10)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = new TestValue(v)
    }
    Cols.fromArray(res)
  }

}

class SigmaExamplesTests extends FunSuite with ContractsTestSuite {

  val backer = new ProveDlogEvidence(g.twice())
  val project = new ProveDlogEvidence(g.threeTimes())
  val selfId = Array[Byte](0, 1)
  val outId = Array[Byte](0, 2)

  test("crowd funding") {
    val timeout = 100
    val minToRaise = 1000
    val contract = new CrowdFundingContract(timeout, minToRaise, backer, project)
    val bytes = Cols.fromArray(Array[Byte]())
    val self = new TestBox(selfId, 10, noBytes, noRegisters)

    { // when backer can open
      val ctxForBacker = new TestContext(noInputs, noOutputs, height = 200, self, Array())
      val ok = contract.canOpen(ctxForBacker)
      assert(ok)
    }

    { // then project can open
      val out = new TestBox(outId, minToRaise, project.propBytes, noRegisters)
      val ctxForProject = new TestContext(Array(), Array(out), height = 50, self, Array())
      val ok = contract.canOpen(ctxForProject)
      assert(ok)
    }
  }

  test("demurrage") {
    val demurragePeriod = 100
    val demurrageCost = 2
    val userProof = new MockProveDlog(isValid = true, noBytes)
    val contract = new DemurrageCurrencyContract(demurragePeriod, demurrageCost, userProof)

    val prop = Cols.fromArray(Array[Byte](1, 2))
    val outHeight = 100L
    val outValue = 10L
    val curHeight = outHeight + demurragePeriod
    val out = new TestBox(outId, outValue, prop, regs(Map(R4 -> curHeight)))

    { //case 1: demurrage time hasn't come yet
      val ctxForProject = new TestContext(
        inputs = Array(),
        outputs = Array(out),
        height = outHeight + demurragePeriod - 1,
        self = new TestBox(
          selfId, outValue, prop,
          regs(Map(R4 -> outHeight))),
        vars = Array()
      )
      userProof.isValid = true
      val userCan = contract.canOpen(ctxForProject)
      assert(userCan)

      userProof.isValid = false
      val minerCan = contract.canOpen(ctxForProject)
      assert(!minerCan)
    }

    { //case 2: demurrage time has come (user can spend all the money)
      val ctxForProject = new TestContext(
        inputs = Array(),
        outputs = Array(out),
        height = outHeight + demurragePeriod,
        self = new TestBox(
          selfId, outValue, prop,
          regs(Map(R4 -> outHeight))),
        vars = Array()
      )
      userProof.isValid = true
      val userCan = contract.canOpen(ctxForProject)
      assert(userCan)
    }

    { //case 3: demurrage time has come (miner can spend "demurrageCost" tokens)
      val minerOut = new TestBox(outId, outValue - demurrageCost, prop, regs(Map(R4 -> curHeight)))
      val ctxForMiner = new TestContext(
        inputs = Array(),
        outputs = Array(minerOut),
        height = outHeight + demurragePeriod,
        self = new TestBox(
          selfId, outValue, prop,
          regs(Map(R4 -> outHeight))),
        vars = Array()
      )
      userProof.isValid = false
      val minerCan = contract.canOpen(ctxForMiner)
      assert(minerCan)
    }
  }
}
