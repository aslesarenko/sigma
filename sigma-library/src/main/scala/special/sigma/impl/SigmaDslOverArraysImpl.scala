package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslOverArraysDefs extends scalan.Scalan with SigmaDslOverArrays {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import TestSigmaDslBuilder._
import SigmaProp._
import SigmaContract._
import Col._
import WArray._
import WOption._
import Box._
import AvlTree._
import AnyValue._
import Col._
import Context._
import ColOverArrayBuilder._
import TrivialSigma._
import WBigInteger._
import WECPoint._
import SigmaDslBuilder._
import DefaultSigma._
import DefaultContract._
import TestBox._
import TestAvlTree._
import TestValue._
import TestContext._
import ProveDlogEvidence._
import ProveDHTEvidence._

object DefaultSigma extends EntityObject("DefaultSigma") {
  // entityProxy: single proxy for each type family
  implicit def proxyDefaultSigma(p: Rep[DefaultSigma]): DefaultSigma = {
    proxyOps[DefaultSigma](p)(scala.reflect.classTag[DefaultSigma])
  }

  // familyElem
  class DefaultSigmaElem[To <: DefaultSigma]
    extends SigmaPropElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaPropElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DefaultSigma].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DefaultSigma] => convertDefaultSigma(x) }
      tryConvert(element[DefaultSigma], this, x, conv)
    }

    def convertDefaultSigma(x: Rep[DefaultSigma]): Rep[To] = {
      x.elem match {
        case _: DefaultSigmaElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DefaultSigmaElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def defaultSigmaElement: Elem[DefaultSigma] =
    cachedElem[DefaultSigmaElem[DefaultSigma]]()

  implicit case object DefaultSigmaCompanionElem extends CompanionElem[DefaultSigmaCompanionCtor] {
    lazy val tag = weakTypeTag[DefaultSigmaCompanionCtor]
    protected def getDefaultRep = RDefaultSigma
  }

  abstract class DefaultSigmaCompanionCtor extends CompanionDef[DefaultSigmaCompanionCtor] with DefaultSigmaCompanion {
    def selfType = DefaultSigmaCompanionElem
    override def toString = "DefaultSigma"
  }
  implicit def proxyDefaultSigmaCompanionCtor(p: Rep[DefaultSigmaCompanionCtor]): DefaultSigmaCompanionCtor =
    proxyOps[DefaultSigmaCompanionCtor](p)

  lazy val RDefaultSigma: Rep[DefaultSigmaCompanionCtor] = new DefaultSigmaCompanionCtor {
  }

  object DefaultSigmaMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[DefaultSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[DefaultSigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DefaultSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[SigmaProp])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[SigmaProp])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "lazyAnd" =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "lazyOr" =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DefaultSigmaCompanionMethods {
  }
} // of object DefaultSigma
  registerEntityObject("DefaultSigma", DefaultSigma)

object DefaultContract extends EntityObject("DefaultContract") {
  // entityProxy: single proxy for each type family
  implicit def proxyDefaultContract(p: Rep[DefaultContract]): DefaultContract = {
    proxyOps[DefaultContract](p)(scala.reflect.classTag[DefaultContract])
  }

  // familyElem
  class DefaultContractElem[To <: DefaultContract]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DefaultContract].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DefaultContract] => convertDefaultContract(x) }
      tryConvert(element[DefaultContract], this, x, conv)
    }

    def convertDefaultContract(x: Rep[DefaultContract]): Rep[To] = {
      x.elem match {
        case _: DefaultContractElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DefaultContractElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def defaultContractElement: Elem[DefaultContract] =
    cachedElem[DefaultContractElem[DefaultContract]]()

  implicit case object DefaultContractCompanionElem extends CompanionElem[DefaultContractCompanionCtor] {
    lazy val tag = weakTypeTag[DefaultContractCompanionCtor]
    protected def getDefaultRep = RDefaultContract
  }

  abstract class DefaultContractCompanionCtor extends CompanionDef[DefaultContractCompanionCtor] with DefaultContractCompanion {
    def selfType = DefaultContractCompanionElem
    override def toString = "DefaultContract"
  }
  implicit def proxyDefaultContractCompanionCtor(p: Rep[DefaultContractCompanionCtor]): DefaultContractCompanionCtor =
    proxyOps[DefaultContractCompanionCtor](p)

  lazy val RDefaultContract: Rep[DefaultContractCompanionCtor] = new DefaultContractCompanionCtor {
  }

  object DefaultContractMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[DefaultContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[DefaultContract]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DefaultContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DefaultContractCompanionMethods {
  }
} // of object DefaultContract
  registerEntityObject("DefaultContract", DefaultContract)

object TestBox extends EntityObject("TestBox") {
  case class TestBoxCtor
      (override val id: Rep[Col[Byte]], override val value: Rep[Long], override val bytes: Rep[Col[Byte]], override val bytesWithoutRef: Rep[Col[Byte]], override val propositionBytes: Rep[Col[Byte]], override val registers: Rep[Col[AnyValue]])
    extends TestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers) with Def[TestBox] {
    lazy val selfType = element[TestBox]
  }
  // elem for concrete class
  class TestBoxElem(val iso: Iso[TestBoxData, TestBox])
    extends BoxElem[TestBox]
    with ConcreteElem[TestBoxData, TestBox] {
    override lazy val parent: Option[Elem[_]] = Some(boxElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertBox(x: Rep[Box]) = RTestBox(x.id, x.value, x.bytes, x.bytesWithoutRef, x.propositionBytes, x.registers)
    override def getDefaultRep = RTestBox(element[Col[Byte]].defaultRepValue, 0l, element[Col[Byte]].defaultRepValue, element[Col[Byte]].defaultRepValue, element[Col[Byte]].defaultRepValue, element[Col[AnyValue]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestBox]
    }
  }

  // state representation type
  type TestBoxData = (Col[Byte], (Long, (Col[Byte], (Col[Byte], (Col[Byte], Col[AnyValue])))))

  // 3) Iso for concrete class
  class TestBoxIso
    extends EntityIso[TestBoxData, TestBox] with Def[TestBoxIso] {
    private lazy val _safeFrom = fun { p: Rep[TestBox] => (p.id, p.value, p.bytes, p.bytesWithoutRef, p.propositionBytes, p.registers) }
    override def from(p: Rep[TestBox]) =
      tryConvert[TestBox, (Col[Byte], (Long, (Col[Byte], (Col[Byte], (Col[Byte], Col[AnyValue])))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Byte], (Long, (Col[Byte], (Col[Byte], (Col[Byte], Col[AnyValue])))))]) = {
      val Pair(id, Pair(value, Pair(bytes, Pair(bytesWithoutRef, Pair(propositionBytes, registers))))) = p
      RTestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers)
    }
    lazy val eFrom = pairElement(element[Col[Byte]], pairElement(element[Long], pairElement(element[Col[Byte]], pairElement(element[Col[Byte]], pairElement(element[Col[Byte]], element[Col[AnyValue]])))))
    lazy val eTo = new TestBoxElem(self)
    lazy val selfType = new TestBoxIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestBoxIsoElem() extends Elem[TestBoxIso] {
    def getDefaultRep = reifyObject(new TestBoxIso())
    lazy val tag = {
      weakTypeTag[TestBoxIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestBoxCompanionCtor extends CompanionDef[TestBoxCompanionCtor] with TestBoxCompanion {
    def selfType = TestBoxCompanionElem
    override def toString = "TestBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestBoxData]): Rep[TestBox] = {
      isoTestBox.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(id: Rep[Col[Byte]], value: Rep[Long], bytes: Rep[Col[Byte]], bytesWithoutRef: Rep[Col[Byte]], propositionBytes: Rep[Col[Byte]], registers: Rep[Col[AnyValue]]): Rep[TestBox] =
      mkTestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers)

    def unapply(p: Rep[Box]) = unmkTestBox(p)
  }
  lazy val TestBoxRep: Rep[TestBoxCompanionCtor] = new TestBoxCompanionCtor
  lazy val RTestBox: TestBoxCompanionCtor = proxyTestBoxCompanion(TestBoxRep)
  implicit def proxyTestBoxCompanion(p: Rep[TestBoxCompanionCtor]): TestBoxCompanionCtor = {
    proxyOps[TestBoxCompanionCtor](p)
  }

  implicit case object TestBoxCompanionElem extends CompanionElem[TestBoxCompanionCtor] {
    lazy val tag = weakTypeTag[TestBoxCompanionCtor]
    protected def getDefaultRep = TestBoxRep
  }

  implicit def proxyTestBox(p: Rep[TestBox]): TestBox =
    proxyOps[TestBox](p)

  implicit class ExtendedTestBox(p: Rep[TestBox]) {
    def toData: Rep[TestBoxData] = {
      isoTestBox.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestBox: Iso[TestBoxData, TestBox] =
    reifyObject(new TestBoxIso())

  def mkTestBox
    (id: Rep[Col[Byte]], value: Rep[Long], bytes: Rep[Col[Byte]], bytesWithoutRef: Rep[Col[Byte]], propositionBytes: Rep[Col[Byte]], registers: Rep[Col[AnyValue]]): Rep[TestBox] = {
    new TestBoxCtor(id, value, bytes, bytesWithoutRef, propositionBytes, registers)
  }
  def unmkTestBox(p: Rep[Box]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestBoxElem @unchecked =>
      Some((p.asRep[TestBox].id, p.asRep[TestBox].value, p.asRep[TestBox].bytes, p.asRep[TestBox].bytesWithoutRef, p.asRep[TestBox].propositionBytes, p.asRep[TestBox].registers))
    case _ =>
      None
  }

    object TestBoxMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[TestBox]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Option[(Rep[TestBox], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, cT, emT, _*), _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "getReg" =>
          Some((receiver, i, cT, emT)).asInstanceOf[Option[(Rep[TestBox], Rep[Int], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestBox], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Option[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[TestBox]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object deserialize {
      def unapply(d: Def[_]): Option[(Rep[TestBox], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, cT, emT, _*), _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "deserialize" =>
          Some((receiver, i, cT, emT)).asInstanceOf[Option[(Rep[TestBox], Rep[Int], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestBox], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestBoxCompanionMethods {
  }
} // of object TestBox
  registerEntityObject("TestBox", TestBox)

object TestAvlTree extends EntityObject("TestAvlTree") {
  case class TestAvlTreeCtor
      (override val startingDigest: Rep[Col[Byte]], override val keyLength: Rep[Int], override val valueLengthOpt: Rep[WOption[Int]], override val maxNumOperations: Rep[WOption[Int]], override val maxDeletes: Rep[WOption[Int]])
    extends TestAvlTree(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes) with Def[TestAvlTree] {
    lazy val selfType = element[TestAvlTree]
  }
  // elem for concrete class
  class TestAvlTreeElem(val iso: Iso[TestAvlTreeData, TestAvlTree])
    extends AvlTreeElem[TestAvlTree]
    with ConcreteElem[TestAvlTreeData, TestAvlTree] {
    override lazy val parent: Option[Elem[_]] = Some(avlTreeElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertAvlTree(x: Rep[AvlTree]) = RTestAvlTree(x.startingDigest, x.keyLength, x.valueLengthOpt, x.maxNumOperations, x.maxDeletes)
    override def getDefaultRep = RTestAvlTree(element[Col[Byte]].defaultRepValue, 0, element[WOption[Int]].defaultRepValue, element[WOption[Int]].defaultRepValue, element[WOption[Int]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestAvlTree]
    }
  }

  // state representation type
  type TestAvlTreeData = (Col[Byte], (Int, (WOption[Int], (WOption[Int], WOption[Int]))))

  // 3) Iso for concrete class
  class TestAvlTreeIso
    extends EntityIso[TestAvlTreeData, TestAvlTree] with Def[TestAvlTreeIso] {
    private lazy val _safeFrom = fun { p: Rep[TestAvlTree] => (p.startingDigest, p.keyLength, p.valueLengthOpt, p.maxNumOperations, p.maxDeletes) }
    override def from(p: Rep[TestAvlTree]) =
      tryConvert[TestAvlTree, (Col[Byte], (Int, (WOption[Int], (WOption[Int], WOption[Int]))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Byte], (Int, (WOption[Int], (WOption[Int], WOption[Int]))))]) = {
      val Pair(startingDigest, Pair(keyLength, Pair(valueLengthOpt, Pair(maxNumOperations, maxDeletes)))) = p
      RTestAvlTree(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)
    }
    lazy val eFrom = pairElement(element[Col[Byte]], pairElement(element[Int], pairElement(element[WOption[Int]], pairElement(element[WOption[Int]], element[WOption[Int]]))))
    lazy val eTo = new TestAvlTreeElem(self)
    lazy val selfType = new TestAvlTreeIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestAvlTreeIsoElem() extends Elem[TestAvlTreeIso] {
    def getDefaultRep = reifyObject(new TestAvlTreeIso())
    lazy val tag = {
      weakTypeTag[TestAvlTreeIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestAvlTreeCompanionCtor extends CompanionDef[TestAvlTreeCompanionCtor] with TestAvlTreeCompanion {
    def selfType = TestAvlTreeCompanionElem
    override def toString = "TestAvlTreeCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestAvlTreeData]): Rep[TestAvlTree] = {
      isoTestAvlTree.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(startingDigest: Rep[Col[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]], maxNumOperations: Rep[WOption[Int]], maxDeletes: Rep[WOption[Int]]): Rep[TestAvlTree] =
      mkTestAvlTree(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)

    def unapply(p: Rep[AvlTree]) = unmkTestAvlTree(p)
  }
  lazy val TestAvlTreeRep: Rep[TestAvlTreeCompanionCtor] = new TestAvlTreeCompanionCtor
  lazy val RTestAvlTree: TestAvlTreeCompanionCtor = proxyTestAvlTreeCompanion(TestAvlTreeRep)
  implicit def proxyTestAvlTreeCompanion(p: Rep[TestAvlTreeCompanionCtor]): TestAvlTreeCompanionCtor = {
    proxyOps[TestAvlTreeCompanionCtor](p)
  }

  implicit case object TestAvlTreeCompanionElem extends CompanionElem[TestAvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[TestAvlTreeCompanionCtor]
    protected def getDefaultRep = TestAvlTreeRep
  }

  implicit def proxyTestAvlTree(p: Rep[TestAvlTree]): TestAvlTree =
    proxyOps[TestAvlTree](p)

  implicit class ExtendedTestAvlTree(p: Rep[TestAvlTree]) {
    def toData: Rep[TestAvlTreeData] = {
      isoTestAvlTree.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestAvlTree: Iso[TestAvlTreeData, TestAvlTree] =
    reifyObject(new TestAvlTreeIso())

  def mkTestAvlTree
    (startingDigest: Rep[Col[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]], maxNumOperations: Rep[WOption[Int]], maxDeletes: Rep[WOption[Int]]): Rep[TestAvlTree] = {
    new TestAvlTreeCtor(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)
  }
  def unmkTestAvlTree(p: Rep[AvlTree]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestAvlTreeElem @unchecked =>
      Some((p.asRep[TestAvlTree].startingDigest, p.asRep[TestAvlTree].keyLength, p.asRep[TestAvlTree].valueLengthOpt, p.asRep[TestAvlTree].maxNumOperations, p.asRep[TestAvlTree].maxDeletes))
    case _ =>
      None
  }

    object TestAvlTreeMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[TestAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestAvlTreeElem] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[TestAvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Option[Rep[TestAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestAvlTreeElem] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[TestAvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestAvlTreeCompanionMethods {
  }
} // of object TestAvlTree
  registerEntityObject("TestAvlTree", TestAvlTree)

object TestValue extends EntityObject("TestValue") {
  case class TestValueCtor[T]
      (override val value: Rep[T])
    extends TestValue[T](value) with Def[TestValue[T]] {
    implicit lazy val eT = value.elem

    lazy val selfType = element[TestValue[T]]
  }
  // elem for concrete class
  class TestValueElem[T](val iso: Iso[TestValueData[T], TestValue[T]])(implicit val eT: Elem[T])
    extends AnyValueElem[TestValue[T]]
    with ConcreteElem[TestValueData[T], TestValue[T]] {
    override lazy val parent: Option[Elem[_]] = Some(anyValueElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override def convertAnyValue(x: Rep[AnyValue]) = // Converter is not generated by meta
!!!("Cannot convert from AnyValue to TestValue: missing fields List(value)")
    override def getDefaultRep = RTestValue(element[T].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[TestValue[T]]
    }
  }

  // state representation type
  type TestValueData[T] = T

  // 3) Iso for concrete class
  class TestValueIso[T](implicit eT: Elem[T])
    extends EntityIso[TestValueData[T], TestValue[T]] with Def[TestValueIso[T]] {
    private lazy val _safeFrom = fun { p: Rep[TestValue[T]] => p.value }
    override def from(p: Rep[TestValue[T]]) =
      tryConvert[TestValue[T], T](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[T]) = {
      val value = p
      RTestValue(value)
    }
    lazy val eFrom = element[T]
    lazy val eTo = new TestValueElem[T](self)
    lazy val selfType = new TestValueIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class TestValueIsoElem[T](eT: Elem[T]) extends Elem[TestValueIso[T]] {
    def getDefaultRep = reifyObject(new TestValueIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[TestValueIso[T]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class TestValueCompanionCtor extends CompanionDef[TestValueCompanionCtor] with TestValueCompanion {
    def selfType = TestValueCompanionElem
    override def toString = "TestValueCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T](value: Rep[T]): Rep[TestValue[T]] =
      mkTestValue(value)

    def unapply[T](p: Rep[AnyValue]) = unmkTestValue(p)
  }
  lazy val TestValueRep: Rep[TestValueCompanionCtor] = new TestValueCompanionCtor
  lazy val RTestValue: TestValueCompanionCtor = proxyTestValueCompanion(TestValueRep)
  implicit def proxyTestValueCompanion(p: Rep[TestValueCompanionCtor]): TestValueCompanionCtor = {
    proxyOps[TestValueCompanionCtor](p)
  }

  implicit case object TestValueCompanionElem extends CompanionElem[TestValueCompanionCtor] {
    lazy val tag = weakTypeTag[TestValueCompanionCtor]
    protected def getDefaultRep = TestValueRep
  }

  implicit def proxyTestValue[T](p: Rep[TestValue[T]]): TestValue[T] =
    proxyOps[TestValue[T]](p)

  implicit class ExtendedTestValue[T](p: Rep[TestValue[T]]) {
    def toData: Rep[TestValueData[T]] = {
      implicit val eT = p.value.elem
      isoTestValue(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestValue[T](implicit eT: Elem[T]): Iso[TestValueData[T], TestValue[T]] =
    reifyObject(new TestValueIso[T]()(eT))

  def mkTestValue[T]
    (value: Rep[T]): Rep[TestValue[T]] = {
    new TestValueCtor[T](value)
  }
  def unmkTestValue[T](p: Rep[AnyValue]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestValueElem[T] @unchecked =>
      Some((p.asRep[TestValue[T]].value))
    case _ =>
      None
  }

    object TestValueMethods {
    object dataSize {
      def unapply(d: Def[_]): Option[Rep[TestValue[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestValueElem[_]] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[TestValue[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestValue[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestValueCompanionMethods {
  }
} // of object TestValue
  registerEntityObject("TestValue", TestValue)

object TestContext extends EntityObject("TestContext") {
  case class TestContextCtor
      (override val inputs: Rep[WArray[Box]], override val outputs: Rep[WArray[Box]], override val height: Rep[Long], override val selfBox: Rep[Box], override val LastBlockUtxoRootHash: Rep[AvlTree], override val vars: Rep[WArray[AnyValue]])
    extends TestContext(inputs, outputs, height, selfBox, LastBlockUtxoRootHash, vars) with Def[TestContext] {
    lazy val selfType = element[TestContext]
  }
  // elem for concrete class
  class TestContextElem(val iso: Iso[TestContextData, TestContext])
    extends ContextElem[TestContext]
    with ConcreteElem[TestContextData, TestContext] {
    override lazy val parent: Option[Elem[_]] = Some(contextElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertContext(x: Rep[Context]) = // Converter is not generated by meta
!!!("Cannot convert from Context to TestContext: missing fields List(inputs, outputs, height, selfBox, vars)")
    override def getDefaultRep = RTestContext(element[WArray[Box]].defaultRepValue, element[WArray[Box]].defaultRepValue, 0l, element[Box].defaultRepValue, element[AvlTree].defaultRepValue, element[WArray[AnyValue]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestContext]
    }
  }

  // state representation type
  type TestContextData = (WArray[Box], (WArray[Box], (Long, (Box, (AvlTree, WArray[AnyValue])))))

  // 3) Iso for concrete class
  class TestContextIso
    extends EntityIso[TestContextData, TestContext] with Def[TestContextIso] {
    private lazy val _safeFrom = fun { p: Rep[TestContext] => (p.inputs, p.outputs, p.height, p.selfBox, p.LastBlockUtxoRootHash, p.vars) }
    override def from(p: Rep[TestContext]) =
      tryConvert[TestContext, (WArray[Box], (WArray[Box], (Long, (Box, (AvlTree, WArray[AnyValue])))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WArray[Box], (WArray[Box], (Long, (Box, (AvlTree, WArray[AnyValue])))))]) = {
      val Pair(inputs, Pair(outputs, Pair(height, Pair(selfBox, Pair(lastBlockUtxoRootHash, vars))))) = p
      RTestContext(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, vars)
    }
    lazy val eFrom = pairElement(element[WArray[Box]], pairElement(element[WArray[Box]], pairElement(element[Long], pairElement(element[Box], pairElement(element[AvlTree], element[WArray[AnyValue]])))))
    lazy val eTo = new TestContextElem(self)
    lazy val selfType = new TestContextIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestContextIsoElem() extends Elem[TestContextIso] {
    def getDefaultRep = reifyObject(new TestContextIso())
    lazy val tag = {
      weakTypeTag[TestContextIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestContextCompanionCtor extends CompanionDef[TestContextCompanionCtor] with TestContextCompanion {
    def selfType = TestContextCompanionElem
    override def toString = "TestContextCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestContextData]): Rep[TestContext] = {
      isoTestContext.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], height: Rep[Long], selfBox: Rep[Box], LastBlockUtxoRootHash: Rep[AvlTree], vars: Rep[WArray[AnyValue]]): Rep[TestContext] =
      mkTestContext(inputs, outputs, height, selfBox, LastBlockUtxoRootHash, vars)

    def unapply(p: Rep[Context]) = unmkTestContext(p)
  }
  lazy val TestContextRep: Rep[TestContextCompanionCtor] = new TestContextCompanionCtor
  lazy val RTestContext: TestContextCompanionCtor = proxyTestContextCompanion(TestContextRep)
  implicit def proxyTestContextCompanion(p: Rep[TestContextCompanionCtor]): TestContextCompanionCtor = {
    proxyOps[TestContextCompanionCtor](p)
  }

  implicit case object TestContextCompanionElem extends CompanionElem[TestContextCompanionCtor] {
    lazy val tag = weakTypeTag[TestContextCompanionCtor]
    protected def getDefaultRep = TestContextRep
  }

  implicit def proxyTestContext(p: Rep[TestContext]): TestContext =
    proxyOps[TestContext](p)

  implicit class ExtendedTestContext(p: Rep[TestContext]) {
    def toData: Rep[TestContextData] = {
      isoTestContext.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestContext: Iso[TestContextData, TestContext] =
    reifyObject(new TestContextIso())

  def mkTestContext
    (inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], height: Rep[Long], selfBox: Rep[Box], LastBlockUtxoRootHash: Rep[AvlTree], vars: Rep[WArray[AnyValue]]): Rep[TestContext] = {
    new TestContextCtor(inputs, outputs, height, selfBox, LastBlockUtxoRootHash, vars)
  }
  def unmkTestContext(p: Rep[Context]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestContextElem @unchecked =>
      Some((p.asRep[TestContext].inputs, p.asRep[TestContext].outputs, p.asRep[TestContext].height, p.asRep[TestContext].selfBox, p.asRep[TestContext].LastBlockUtxoRootHash, p.asRep[TestContext].vars))
    case _ =>
      None
  }

    object TestContextMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "HEIGHT" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "SELF" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "INPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "OUTPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Option[(Rep[TestContext], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(id, cT, emT, _*), _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "getVar" =>
          Some((receiver, id, cT, emT)).asInstanceOf[Option[(Rep[TestContext], Rep[Byte], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestContext], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object deserialize {
      def unapply(d: Def[_]): Option[(Rep[TestContext], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(id, cT, emT, _*), _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "deserialize" =>
          Some((receiver, id, cT, emT)).asInstanceOf[Option[(Rep[TestContext], Rep[Byte], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestContext], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestContextCompanionMethods {
  }
} // of object TestContext
  registerEntityObject("TestContext", TestContext)

object TestSigmaDslBuilder extends EntityObject("TestSigmaDslBuilder") {
  case class TestSigmaDslBuilderCtor
      ()
    extends TestSigmaDslBuilder() with Def[TestSigmaDslBuilder] {
    lazy val selfType = element[TestSigmaDslBuilder]
  }
  // elem for concrete class
  class TestSigmaDslBuilderElem(val iso: Iso[TestSigmaDslBuilderData, TestSigmaDslBuilder])
    extends SigmaDslBuilderElem[TestSigmaDslBuilder]
    with ConcreteElem[TestSigmaDslBuilderData, TestSigmaDslBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaDslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSigmaDslBuilder(x: Rep[SigmaDslBuilder]) = RTestSigmaDslBuilder()
    override def getDefaultRep = RTestSigmaDslBuilder()
    override lazy val tag = {
      weakTypeTag[TestSigmaDslBuilder]
    }
  }

  // state representation type
  type TestSigmaDslBuilderData = Unit

  // 3) Iso for concrete class
  class TestSigmaDslBuilderIso
    extends EntityIso[TestSigmaDslBuilderData, TestSigmaDslBuilder] with Def[TestSigmaDslBuilderIso] {
    private lazy val _safeFrom = fun { p: Rep[TestSigmaDslBuilder] => () }
    override def from(p: Rep[TestSigmaDslBuilder]) =
      tryConvert[TestSigmaDslBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RTestSigmaDslBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new TestSigmaDslBuilderElem(self)
    lazy val selfType = new TestSigmaDslBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestSigmaDslBuilderIsoElem() extends Elem[TestSigmaDslBuilderIso] {
    def getDefaultRep = reifyObject(new TestSigmaDslBuilderIso())
    lazy val tag = {
      weakTypeTag[TestSigmaDslBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestSigmaDslBuilderCompanionCtor extends CompanionDef[TestSigmaDslBuilderCompanionCtor] with TestSigmaDslBuilderCompanion {
    def selfType = TestSigmaDslBuilderCompanionElem
    override def toString = "TestSigmaDslBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestSigmaDslBuilderData]): Rep[TestSigmaDslBuilder] = {
      isoTestSigmaDslBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[TestSigmaDslBuilder] =
      mkTestSigmaDslBuilder()

    def unapply(p: Rep[SigmaDslBuilder]) = unmkTestSigmaDslBuilder(p)
  }
  lazy val TestSigmaDslBuilderRep: Rep[TestSigmaDslBuilderCompanionCtor] = new TestSigmaDslBuilderCompanionCtor
  lazy val RTestSigmaDslBuilder: TestSigmaDslBuilderCompanionCtor = proxyTestSigmaDslBuilderCompanion(TestSigmaDslBuilderRep)
  implicit def proxyTestSigmaDslBuilderCompanion(p: Rep[TestSigmaDslBuilderCompanionCtor]): TestSigmaDslBuilderCompanionCtor = {
    proxyOps[TestSigmaDslBuilderCompanionCtor](p)
  }

  implicit case object TestSigmaDslBuilderCompanionElem extends CompanionElem[TestSigmaDslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[TestSigmaDslBuilderCompanionCtor]
    protected def getDefaultRep = TestSigmaDslBuilderRep
  }

  implicit def proxyTestSigmaDslBuilder(p: Rep[TestSigmaDslBuilder]): TestSigmaDslBuilder =
    proxyOps[TestSigmaDslBuilder](p)

  implicit class ExtendedTestSigmaDslBuilder(p: Rep[TestSigmaDslBuilder]) {
    def toData: Rep[TestSigmaDslBuilderData] = {
      isoTestSigmaDslBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestSigmaDslBuilder: Iso[TestSigmaDslBuilderData, TestSigmaDslBuilder] =
    reifyObject(new TestSigmaDslBuilderIso())

  def mkTestSigmaDslBuilder
    (): Rep[TestSigmaDslBuilder] = {
    new TestSigmaDslBuilderCtor()
  }
  def unmkTestSigmaDslBuilder(p: Rep[SigmaDslBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestSigmaDslBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object TestSigmaDslBuilderMethods {
    object Cols {
      def unapply(d: Def[_]): Option[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "Cols" =>
          Some(receiver).asInstanceOf[Option[Rep[TestSigmaDslBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(proof, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "verifyZK" =>
          Some((receiver, proof)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(bound, props, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "atLeast" =>
          Some((receiver, bound, props)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "allOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "anyOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(proofs, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "allZK" =>
          Some((receiver, proofs)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(proofs, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "anyZK" =>
          Some((receiver, proofs)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "sigmaProp" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "blake2b256" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "sha256" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[String])] = d match {
        case MethodCall(receiver, method, Seq(base64String, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "PubKey" =>
          Some((receiver, base64String)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[String])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "byteArrayToBigInt" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[Long])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "longToByteArray" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[Long])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(g, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "proveDlog" =>
          Some((receiver, g)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(g, h, u, v, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "proveDHTuple" =>
          Some((receiver, g, h, u, v)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isMember {
      def unapply(d: Def[_]): Option[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(tree, key, proof, _*), _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "isMember" =>
          Some((receiver, tree, key, proof)).asInstanceOf[Option[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Option[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "groupGenerator" =>
          Some(receiver).asInstanceOf[Option[Rep[TestSigmaDslBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestSigmaDslBuilderCompanionMethods {
  }
} // of object TestSigmaDslBuilder
  registerEntityObject("TestSigmaDslBuilder", TestSigmaDslBuilder)

object TrivialSigma extends EntityObject("TrivialSigma") {
  case class TrivialSigmaCtor
      (override val isValid: Rep[Boolean])
    extends TrivialSigma(isValid) with Def[TrivialSigma] {
    lazy val selfType = element[TrivialSigma]
  }
  // elem for concrete class
  class TrivialSigmaElem(val iso: Iso[TrivialSigmaData, TrivialSigma])
    extends DefaultSigmaElem[TrivialSigma]
    with ConcreteElem[TrivialSigmaData, TrivialSigma] {
    override lazy val parent: Option[Elem[_]] = Some(defaultSigmaElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertDefaultSigma(x: Rep[DefaultSigma]) = RTrivialSigma(x.isValid)
    override def getDefaultRep = RTrivialSigma(false)
    override lazy val tag = {
      weakTypeTag[TrivialSigma]
    }
  }

  // state representation type
  type TrivialSigmaData = Boolean

  // 3) Iso for concrete class
  class TrivialSigmaIso
    extends EntityIso[TrivialSigmaData, TrivialSigma] with Def[TrivialSigmaIso] {
    private lazy val _safeFrom = fun { p: Rep[TrivialSigma] => p.isValid }
    override def from(p: Rep[TrivialSigma]) =
      tryConvert[TrivialSigma, Boolean](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Boolean]) = {
      val isValid = p
      RTrivialSigma(isValid)
    }
    lazy val eFrom = element[Boolean]
    lazy val eTo = new TrivialSigmaElem(self)
    lazy val selfType = new TrivialSigmaIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TrivialSigmaIsoElem() extends Elem[TrivialSigmaIso] {
    def getDefaultRep = reifyObject(new TrivialSigmaIso())
    lazy val tag = {
      weakTypeTag[TrivialSigmaIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TrivialSigmaCompanionCtor extends CompanionDef[TrivialSigmaCompanionCtor] with TrivialSigmaCompanion {
    def selfType = TrivialSigmaCompanionElem
    override def toString = "TrivialSigmaCompanion"

    @scalan.OverloadId("fromFields")
    def apply(isValid: Rep[Boolean]): Rep[TrivialSigma] =
      mkTrivialSigma(isValid)

    def unapply(p: Rep[DefaultSigma]) = unmkTrivialSigma(p)
  }
  lazy val TrivialSigmaRep: Rep[TrivialSigmaCompanionCtor] = new TrivialSigmaCompanionCtor
  lazy val RTrivialSigma: TrivialSigmaCompanionCtor = proxyTrivialSigmaCompanion(TrivialSigmaRep)
  implicit def proxyTrivialSigmaCompanion(p: Rep[TrivialSigmaCompanionCtor]): TrivialSigmaCompanionCtor = {
    proxyOps[TrivialSigmaCompanionCtor](p)
  }

  implicit case object TrivialSigmaCompanionElem extends CompanionElem[TrivialSigmaCompanionCtor] {
    lazy val tag = weakTypeTag[TrivialSigmaCompanionCtor]
    protected def getDefaultRep = TrivialSigmaRep
  }

  implicit def proxyTrivialSigma(p: Rep[TrivialSigma]): TrivialSigma =
    proxyOps[TrivialSigma](p)

  implicit class ExtendedTrivialSigma(p: Rep[TrivialSigma]) {
    def toData: Rep[TrivialSigmaData] = {
      isoTrivialSigma.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTrivialSigma: Iso[TrivialSigmaData, TrivialSigma] =
    reifyObject(new TrivialSigmaIso())

  def mkTrivialSigma
    (isValid: Rep[Boolean]): Rep[TrivialSigma] = {
    new TrivialSigmaCtor(isValid)
  }
  def unmkTrivialSigma(p: Rep[DefaultSigma]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TrivialSigmaElem @unchecked =>
      Some((p.asRep[TrivialSigma].isValid))
    case _ =>
      None
  }

    object TrivialSigmaMethods {
    object propBytes {
      def unapply(d: Def[_]): Option[Rep[TrivialSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[TrivialSigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TrivialSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TrivialSigmaCompanionMethods {
  }
} // of object TrivialSigma
  registerEntityObject("TrivialSigma", TrivialSigma)

object ProveDlogEvidence extends EntityObject("ProveDlogEvidence") {
  case class ProveDlogEvidenceCtor
      (override val value: Rep[WECPoint])
    extends ProveDlogEvidence(value) with Def[ProveDlogEvidence] {
    lazy val selfType = element[ProveDlogEvidence]
  }
  // elem for concrete class
  class ProveDlogEvidenceElem(val iso: Iso[ProveDlogEvidenceData, ProveDlogEvidence])
    extends DefaultSigmaElem[ProveDlogEvidence]
    with ConcreteElem[ProveDlogEvidenceData, ProveDlogEvidence] {
    override lazy val parent: Option[Elem[_]] = Some(defaultSigmaElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertDefaultSigma(x: Rep[DefaultSigma]) = // Converter is not generated by meta
!!!("Cannot convert from DefaultSigma to ProveDlogEvidence: missing fields List(value)")
    override def getDefaultRep = RProveDlogEvidence(element[WECPoint].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[ProveDlogEvidence]
    }
  }

  // state representation type
  type ProveDlogEvidenceData = WECPoint

  // 3) Iso for concrete class
  class ProveDlogEvidenceIso
    extends EntityIso[ProveDlogEvidenceData, ProveDlogEvidence] with Def[ProveDlogEvidenceIso] {
    private lazy val _safeFrom = fun { p: Rep[ProveDlogEvidence] => p.value }
    override def from(p: Rep[ProveDlogEvidence]) =
      tryConvert[ProveDlogEvidence, WECPoint](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WECPoint]) = {
      val value = p
      RProveDlogEvidence(value)
    }
    lazy val eFrom = element[WECPoint]
    lazy val eTo = new ProveDlogEvidenceElem(self)
    lazy val selfType = new ProveDlogEvidenceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ProveDlogEvidenceIsoElem() extends Elem[ProveDlogEvidenceIso] {
    def getDefaultRep = reifyObject(new ProveDlogEvidenceIso())
    lazy val tag = {
      weakTypeTag[ProveDlogEvidenceIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ProveDlogEvidenceCompanionCtor extends CompanionDef[ProveDlogEvidenceCompanionCtor] with ProveDlogEvidenceCompanion {
    def selfType = ProveDlogEvidenceCompanionElem
    override def toString = "ProveDlogEvidenceCompanion"

    @scalan.OverloadId("fromFields")
    def apply(value: Rep[WECPoint]): Rep[ProveDlogEvidence] =
      mkProveDlogEvidence(value)

    def unapply(p: Rep[DefaultSigma]) = unmkProveDlogEvidence(p)
  }
  lazy val ProveDlogEvidenceRep: Rep[ProveDlogEvidenceCompanionCtor] = new ProveDlogEvidenceCompanionCtor
  lazy val RProveDlogEvidence: ProveDlogEvidenceCompanionCtor = proxyProveDlogEvidenceCompanion(ProveDlogEvidenceRep)
  implicit def proxyProveDlogEvidenceCompanion(p: Rep[ProveDlogEvidenceCompanionCtor]): ProveDlogEvidenceCompanionCtor = {
    proxyOps[ProveDlogEvidenceCompanionCtor](p)
  }

  implicit case object ProveDlogEvidenceCompanionElem extends CompanionElem[ProveDlogEvidenceCompanionCtor] {
    lazy val tag = weakTypeTag[ProveDlogEvidenceCompanionCtor]
    protected def getDefaultRep = ProveDlogEvidenceRep
  }

  implicit def proxyProveDlogEvidence(p: Rep[ProveDlogEvidence]): ProveDlogEvidence =
    proxyOps[ProveDlogEvidence](p)

  implicit class ExtendedProveDlogEvidence(p: Rep[ProveDlogEvidence]) {
    def toData: Rep[ProveDlogEvidenceData] = {
      isoProveDlogEvidence.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoProveDlogEvidence: Iso[ProveDlogEvidenceData, ProveDlogEvidence] =
    reifyObject(new ProveDlogEvidenceIso())

  def mkProveDlogEvidence
    (value: Rep[WECPoint]): Rep[ProveDlogEvidence] = {
    new ProveDlogEvidenceCtor(value)
  }
  def unmkProveDlogEvidence(p: Rep[DefaultSigma]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ProveDlogEvidenceElem @unchecked =>
      Some((p.asRep[ProveDlogEvidence].value))
    case _ =>
      None
  }

    object ProveDlogEvidenceMethods {
    object propBytes {
      def unapply(d: Def[_]): Option[Rep[ProveDlogEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[ProveDlogEvidence]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ProveDlogEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isValid {
      def unapply(d: Def[_]): Option[Rep[ProveDlogEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "isValid" =>
          Some(receiver).asInstanceOf[Option[Rep[ProveDlogEvidence]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ProveDlogEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ProveDlogEvidenceCompanionMethods {
  }
} // of object ProveDlogEvidence
  registerEntityObject("ProveDlogEvidence", ProveDlogEvidence)

object ProveDHTEvidence extends EntityObject("ProveDHTEvidence") {
  case class ProveDHTEvidenceCtor
      (override val value: Rep[WECPoint])
    extends ProveDHTEvidence(value) with Def[ProveDHTEvidence] {
    lazy val selfType = element[ProveDHTEvidence]
  }
  // elem for concrete class
  class ProveDHTEvidenceElem(val iso: Iso[ProveDHTEvidenceData, ProveDHTEvidence])
    extends DefaultSigmaElem[ProveDHTEvidence]
    with ConcreteElem[ProveDHTEvidenceData, ProveDHTEvidence] {
    override lazy val parent: Option[Elem[_]] = Some(defaultSigmaElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertDefaultSigma(x: Rep[DefaultSigma]) = // Converter is not generated by meta
!!!("Cannot convert from DefaultSigma to ProveDHTEvidence: missing fields List(value)")
    override def getDefaultRep = RProveDHTEvidence(element[WECPoint].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[ProveDHTEvidence]
    }
  }

  // state representation type
  type ProveDHTEvidenceData = WECPoint

  // 3) Iso for concrete class
  class ProveDHTEvidenceIso
    extends EntityIso[ProveDHTEvidenceData, ProveDHTEvidence] with Def[ProveDHTEvidenceIso] {
    private lazy val _safeFrom = fun { p: Rep[ProveDHTEvidence] => p.value }
    override def from(p: Rep[ProveDHTEvidence]) =
      tryConvert[ProveDHTEvidence, WECPoint](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WECPoint]) = {
      val value = p
      RProveDHTEvidence(value)
    }
    lazy val eFrom = element[WECPoint]
    lazy val eTo = new ProveDHTEvidenceElem(self)
    lazy val selfType = new ProveDHTEvidenceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ProveDHTEvidenceIsoElem() extends Elem[ProveDHTEvidenceIso] {
    def getDefaultRep = reifyObject(new ProveDHTEvidenceIso())
    lazy val tag = {
      weakTypeTag[ProveDHTEvidenceIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ProveDHTEvidenceCompanionCtor extends CompanionDef[ProveDHTEvidenceCompanionCtor] with ProveDHTEvidenceCompanion {
    def selfType = ProveDHTEvidenceCompanionElem
    override def toString = "ProveDHTEvidenceCompanion"

    @scalan.OverloadId("fromFields")
    def apply(value: Rep[WECPoint]): Rep[ProveDHTEvidence] =
      mkProveDHTEvidence(value)

    def unapply(p: Rep[DefaultSigma]) = unmkProveDHTEvidence(p)
  }
  lazy val ProveDHTEvidenceRep: Rep[ProveDHTEvidenceCompanionCtor] = new ProveDHTEvidenceCompanionCtor
  lazy val RProveDHTEvidence: ProveDHTEvidenceCompanionCtor = proxyProveDHTEvidenceCompanion(ProveDHTEvidenceRep)
  implicit def proxyProveDHTEvidenceCompanion(p: Rep[ProveDHTEvidenceCompanionCtor]): ProveDHTEvidenceCompanionCtor = {
    proxyOps[ProveDHTEvidenceCompanionCtor](p)
  }

  implicit case object ProveDHTEvidenceCompanionElem extends CompanionElem[ProveDHTEvidenceCompanionCtor] {
    lazy val tag = weakTypeTag[ProveDHTEvidenceCompanionCtor]
    protected def getDefaultRep = ProveDHTEvidenceRep
  }

  implicit def proxyProveDHTEvidence(p: Rep[ProveDHTEvidence]): ProveDHTEvidence =
    proxyOps[ProveDHTEvidence](p)

  implicit class ExtendedProveDHTEvidence(p: Rep[ProveDHTEvidence]) {
    def toData: Rep[ProveDHTEvidenceData] = {
      isoProveDHTEvidence.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoProveDHTEvidence: Iso[ProveDHTEvidenceData, ProveDHTEvidence] =
    reifyObject(new ProveDHTEvidenceIso())

  def mkProveDHTEvidence
    (value: Rep[WECPoint]): Rep[ProveDHTEvidence] = {
    new ProveDHTEvidenceCtor(value)
  }
  def unmkProveDHTEvidence(p: Rep[DefaultSigma]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ProveDHTEvidenceElem @unchecked =>
      Some((p.asRep[ProveDHTEvidence].value))
    case _ =>
      None
  }

    object ProveDHTEvidenceMethods {
    object propBytes {
      def unapply(d: Def[_]): Option[Rep[ProveDHTEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[ProveDHTEvidence]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ProveDHTEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isValid {
      def unapply(d: Def[_]): Option[Rep[ProveDHTEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "isValid" =>
          Some(receiver).asInstanceOf[Option[Rep[ProveDHTEvidence]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ProveDHTEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ProveDHTEvidenceCompanionMethods {
  }
} // of object ProveDHTEvidence
  registerEntityObject("ProveDHTEvidence", ProveDHTEvidence)

  registerModule(SigmaDslOverArraysModule)
}

object SigmaDslOverArraysModule extends scalan.ModuleInfo("special.sigma", "SigmaDslOverArrays")
}

trait SigmaDslOverArraysModule extends special.sigma.impl.SigmaDslOverArraysDefs {self: SigmaLibrary =>}
