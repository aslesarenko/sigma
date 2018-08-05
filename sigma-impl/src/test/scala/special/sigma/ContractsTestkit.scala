package special.sigma

import special.collection.{Col, ColOverArrayBuilder}
import org.bouncycastle.crypto.ec.CustomNamedCurves

trait ContractsTestkit {
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
