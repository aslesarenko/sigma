package special.sigma

import special.collection.{Col, ColOverArrayBuilder}
import org.bouncycastle.crypto.ec.CustomNamedCurves

import scala.reflect.ClassTag

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


  val Cols = new ColOverArrayBuilder
  val SigmaDsl = new TestSigmaDslBuilder
  val noRegisters = collection[AnyValue]()
  val noBytes = collection[Byte]()
  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()
  val emptyAvlTree = new TestAvlTree(noBytes, 0, None, None, None)

  def collection[T:ClassTag](items: T*) = Cols.fromArray(items.toArray)

  def regs(m: Map[Byte, Any]): Col[AnyValue] = {
    val res = new Array[AnyValue](10)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = new TestValue(v)
    }
    Cols.fromArray(res)
  }

  def contextVars(m: Map[Byte, Any]): Col[AnyValue] = {
    val res = new Array[AnyValue](m.keys.max)
    for ((id, v) <- m) {
      val i = id - 1
      assert(res(i) == null, s"register $id is defined more then once")
      res(i) = new TestValue(v)
    }
    Cols.fromArray(res)
  }

  implicit def boolToSigma(b: Boolean): Sigma = TrivialSigma(b)
}
