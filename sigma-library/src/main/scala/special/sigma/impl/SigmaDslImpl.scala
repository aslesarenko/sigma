package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import scalan.OverloadHack.Overloaded1

  // Abs -----------------------------------
trait SigmaDslDefs extends scalan.Scalan with SigmaDsl {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import DslBuilder._
import SigmaDslBuilder._
import DslObject._
import Col._
import SigmaProp._
import AnyValue._
import WOption._
import Box._
import AvlTree._
import Context._
import SigmaContract._
import ColBuilder._

object DslBuilder extends EntityObject("DslBuilder") {
  // entityProxy: single proxy for each type family
  implicit def proxyDslBuilder(p: Rep[DslBuilder]): DslBuilder = {
    proxyOps[DslBuilder](p)(scala.reflect.classTag[DslBuilder])
  }

  // familyElem
  class DslBuilderElem[To <: DslBuilder]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DslBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DslBuilder] => convertDslBuilder(x) }
      tryConvert(element[DslBuilder], this, x, conv)
    }

    def convertDslBuilder(x: Rep[DslBuilder]): Rep[To] = {
      x.elem match {
        case _: DslBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DslBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def dslBuilderElement: Elem[DslBuilder] =
    cachedElem[DslBuilderElem[DslBuilder]]()

  implicit case object DslBuilderCompanionElem extends CompanionElem[DslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[DslBuilderCompanionCtor]
    protected def getDefaultRep = RDslBuilder
  }

  abstract class DslBuilderCompanionCtor extends CompanionDef[DslBuilderCompanionCtor] with DslBuilderCompanion {
    def selfType = DslBuilderCompanionElem
    override def toString = "DslBuilder"
  }
  implicit def proxyDslBuilderCompanionCtor(p: Rep[DslBuilderCompanionCtor]): DslBuilderCompanionCtor =
    proxyOps[DslBuilderCompanionCtor](p)

  lazy val RDslBuilder: Rep[DslBuilderCompanionCtor] = new DslBuilderCompanionCtor {
  }

  object DslBuilderMethods {
  }

  object DslBuilderCompanionMethods {
  }
} // of object DslBuilder
  registerEntityObject("DslBuilder", DslBuilder)

object DslObject extends EntityObject("DslObject") {
  // entityProxy: single proxy for each type family
  implicit def proxyDslObject(p: Rep[DslObject]): DslObject = {
    proxyOps[DslObject](p)(scala.reflect.classTag[DslObject])
  }

  // familyElem
  class DslObjectElem[To <: DslObject]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DslObject].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DslObject] => convertDslObject(x) }
      tryConvert(element[DslObject], this, x, conv)
    }

    def convertDslObject(x: Rep[DslObject]): Rep[To] = {
      x.elem match {
        case _: DslObjectElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DslObjectElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def dslObjectElement: Elem[DslObject] =
    cachedElem[DslObjectElem[DslObject]]()

  implicit case object DslObjectCompanionElem extends CompanionElem[DslObjectCompanionCtor] {
    lazy val tag = weakTypeTag[DslObjectCompanionCtor]
    protected def getDefaultRep = RDslObject
  }

  abstract class DslObjectCompanionCtor extends CompanionDef[DslObjectCompanionCtor] with DslObjectCompanion {
    def selfType = DslObjectCompanionElem
    override def toString = "DslObject"
  }
  implicit def proxyDslObjectCompanionCtor(p: Rep[DslObjectCompanionCtor]): DslObjectCompanionCtor =
    proxyOps[DslObjectCompanionCtor](p)

  lazy val RDslObject: Rep[DslObjectCompanionCtor] = new DslObjectCompanionCtor {
  }

  object DslObjectMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[DslObject]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DslObjectElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[DslObject]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DslObject]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DslObjectCompanionMethods {
  }
} // of object DslObject
  registerEntityObject("DslObject", DslObject)

object SigmaProp extends EntityObject("SigmaProp") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaProp = special.sigma.SigmaProp
  case class SigmaPropConst(
        constValue: SSigmaProp
      ) extends SigmaProp with LiftedConst[SSigmaProp, SigmaProp] {
    val liftable: Liftable[SSigmaProp, SigmaProp] = LiftableSigmaProp
    val selfType: Elem[SigmaProp] = liftable.eW
    def builder: Rep[SigmaDslBuilder] = delayInvoke
    def isValid: Rep[Boolean] = delayInvoke
    def propBytes: Rep[Col[Byte]] = delayInvoke
    @OverloadId(value = "and_sigma") def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke
    @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke
    @OverloadId(value = "or_sigma") def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke
    @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke
    def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke
  }

  implicit object LiftableSigmaProp
    extends Liftable[SSigmaProp, SigmaProp] {
    lazy val eW: Elem[SigmaProp] = sigmaPropElement
    lazy val sourceClassTag: ClassTag[SSigmaProp] = {
      classTag[SSigmaProp]
    }
    def lift(x: SSigmaProp): Rep[SigmaProp] = SigmaPropConst(x)
    def unlift(w: Rep[SigmaProp]): SSigmaProp = w match {
      case Def(SigmaPropConst(x: SSigmaProp))
            => x.asInstanceOf[SSigmaProp]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaProp(p: Rep[SigmaProp]): SigmaProp = {
    proxyOps[SigmaProp](p)(scala.reflect.classTag[SigmaProp])
  }

  // familyElem
  class SigmaPropElem[To <: SigmaProp]
    extends DslObjectElem[To] {
    override val liftable = LiftableSigmaProp.asLiftable[SSigmaProp, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[SigmaProp], classOf[SSigmaProp], Set(
        "isValid", "propBytes", "$amp$amp", "$amp$amp", "$bar$bar", "$bar$bar", "lazyAnd", "lazyOr"
      ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslObjectElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaProp].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaProp] => convertSigmaProp(x) }
      tryConvert(element[SigmaProp], this, x, conv)
    }

    def convertSigmaProp(x: Rep[SigmaProp]): Rep[To] = {
      x.elem match {
        case _: SigmaPropElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SigmaPropElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaPropElement: Elem[SigmaProp] =
    cachedElem[SigmaPropElem[SigmaProp]]()

  implicit case object SigmaPropCompanionElem extends CompanionElem[SigmaPropCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaPropCompanionCtor]
    protected def getDefaultRep = RSigmaProp
  }

  abstract class SigmaPropCompanionCtor extends CompanionDef[SigmaPropCompanionCtor] with SigmaPropCompanion {
    def selfType = SigmaPropCompanionElem
    override def toString = "SigmaProp"
  }
  implicit def proxySigmaPropCompanionCtor(p: Rep[SigmaPropCompanionCtor]): SigmaPropCompanionCtor =
    proxyOps[SigmaPropCompanionCtor](p)

  lazy val RSigmaProp: Rep[SigmaPropCompanionCtor] = new SigmaPropCompanionCtor {
  }

  object SigmaPropMethods {
    object isValid {
      def unapply(d: Def[_]): Option[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "isValid" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaProp]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object propBytes {
      def unapply(d: Def[_]): Option[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaProp]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Option[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaProp], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Option[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaProp], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Option[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaProp], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Option[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaProp], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Option[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "lazyAnd" =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Option[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "lazyOr" =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SigmaPropCompanionMethods {
  }
} // of object SigmaProp
  registerEntityObject("SigmaProp", SigmaProp)

object AnyValue extends EntityObject("AnyValue") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SAnyValue = special.sigma.AnyValue
  case class AnyValueConst(
        constValue: SAnyValue
      ) extends AnyValue with LiftedConst[SAnyValue, AnyValue] {
    val liftable: Liftable[SAnyValue, AnyValue] = LiftableAnyValue
    val selfType: Elem[AnyValue] = liftable.eW
    def dataSize: Rep[Long] = delayInvoke
  }

  implicit object LiftableAnyValue
    extends Liftable[SAnyValue, AnyValue] {
    lazy val eW: Elem[AnyValue] = anyValueElement
    lazy val sourceClassTag: ClassTag[SAnyValue] = {
      classTag[SAnyValue]
    }
    def lift(x: SAnyValue): Rep[AnyValue] = AnyValueConst(x)
    def unlift(w: Rep[AnyValue]): SAnyValue = w match {
      case Def(AnyValueConst(x: SAnyValue))
            => x.asInstanceOf[SAnyValue]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAnyValue(p: Rep[AnyValue]): AnyValue = {
    proxyOps[AnyValue](p)(scala.reflect.classTag[AnyValue])
  }

  // familyElem
  class AnyValueElem[To <: AnyValue]
    extends EntityElem[To] {
    override val liftable = LiftableAnyValue.asLiftable[SAnyValue, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[AnyValue], classOf[SAnyValue], Set(
        "dataSize"
      ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[AnyValue].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[AnyValue] => convertAnyValue(x) }
      tryConvert(element[AnyValue], this, x, conv)
    }

    def convertAnyValue(x: Rep[AnyValue]): Rep[To] = {
      x.elem match {
        case _: AnyValueElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have AnyValueElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def anyValueElement: Elem[AnyValue] =
    cachedElem[AnyValueElem[AnyValue]]()

  implicit case object AnyValueCompanionElem extends CompanionElem[AnyValueCompanionCtor] {
    lazy val tag = weakTypeTag[AnyValueCompanionCtor]
    protected def getDefaultRep = RAnyValue
  }

  abstract class AnyValueCompanionCtor extends CompanionDef[AnyValueCompanionCtor] with AnyValueCompanion {
    def selfType = AnyValueCompanionElem
    override def toString = "AnyValue"
  }
  implicit def proxyAnyValueCompanionCtor(p: Rep[AnyValueCompanionCtor]): AnyValueCompanionCtor =
    proxyOps[AnyValueCompanionCtor](p)

  lazy val RAnyValue: Rep[AnyValueCompanionCtor] = new AnyValueCompanionCtor {
  }

  object AnyValueMethods {
    object dataSize {
      def unapply(d: Def[_]): Option[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AnyValueElem[_]] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[AnyValue]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AnyValueCompanionMethods {
  }
} // of object AnyValue
  registerEntityObject("AnyValue", AnyValue)

object Box extends EntityObject("Box") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SBox = special.sigma.Box
  case class BoxConst(
        constValue: SBox
      ) extends Box with LiftedConst[SBox, Box] {
    val liftable: Liftable[SBox, Box] = LiftableBox
    val selfType: Elem[Box] = liftable.eW
    def builder: Rep[SigmaDslBuilder] = delayInvoke
    def id: Rep[Col[Byte]] = delayInvoke
    def value: Rep[Long] = delayInvoke
    def bytes: Rep[Col[Byte]] = delayInvoke
    def bytesWithoutRef: Rep[Col[Byte]] = delayInvoke
    def propositionBytes: Rep[Col[Byte]] = delayInvoke
    def dataSize: Rep[Long] = delayInvoke
    def registers: Rep[Col[AnyValue]] = delayInvoke
    def deserialize[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = delayInvoke
    def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = delayInvoke
  }

  implicit object LiftableBox
    extends Liftable[SBox, Box] {
    lazy val eW: Elem[Box] = boxElement
    lazy val sourceClassTag: ClassTag[SBox] = {
      classTag[SBox]
    }
    def lift(x: SBox): Rep[Box] = BoxConst(x)
    def unlift(w: Rep[Box]): SBox = w match {
      case Def(BoxConst(x: SBox))
            => x.asInstanceOf[SBox]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBox(p: Rep[Box]): Box = {
    proxyOps[Box](p)(scala.reflect.classTag[Box])
  }

  // familyElem
  class BoxElem[To <: Box]
    extends DslObjectElem[To] {
    override val liftable = LiftableBox.asLiftable[SBox, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[Box], classOf[SBox], Set(
        "id", "value", "bytes", "bytesWithoutRef", "propositionBytes", "dataSize", "registers", "deserialize", "getReg", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "tokens"
      ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslObjectElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Box].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Box] => convertBox(x) }
      tryConvert(element[Box], this, x, conv)
    }

    def convertBox(x: Rep[Box]): Rep[To] = {
      x.elem match {
        case _: BoxElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have BoxElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def boxElement: Elem[Box] =
    cachedElem[BoxElem[Box]]()

  implicit case object BoxCompanionElem extends CompanionElem[BoxCompanionCtor] {
    lazy val tag = weakTypeTag[BoxCompanionCtor]
    protected def getDefaultRep = RBox
  }

  abstract class BoxCompanionCtor extends CompanionDef[BoxCompanionCtor] with BoxCompanion {
    def selfType = BoxCompanionElem
    override def toString = "Box"
  }
  implicit def proxyBoxCompanionCtor(p: Rep[BoxCompanionCtor]): BoxCompanionCtor =
    proxyOps[BoxCompanionCtor](p)

  lazy val RBox: Rep[BoxCompanionCtor] = new BoxCompanionCtor {
  }

  object BoxMethods {
    object id {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "id" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bytes {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "bytes" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "bytesWithoutRef" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object propositionBytes {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "propositionBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object registers {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "registers" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object deserialize {
      def unapply(d: Def[_]): Option[(Rep[Box], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "deserialize" =>
          Some((receiver, i, cT, emT)).asInstanceOf[Option[(Rep[Box], Rep[Int], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Option[(Rep[Box], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "getReg" =>
          Some((receiver, i, cT, emT)).asInstanceOf[Option[(Rep[Box], Rep[Int], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Rep[Int], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R0 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R0" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R1 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R1" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R2 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R2" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R3 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R3" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R4 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R4" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R5 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R5" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R6 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R6" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R7 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R7" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R8 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R8" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R9 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(cT, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R9" =>
          Some((receiver, cT, emT)).asInstanceOf[Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object tokens {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "tokens" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BoxCompanionMethods {
  }
} // of object Box
  registerEntityObject("Box", Box)

object AvlTree extends EntityObject("AvlTree") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SAvlTree = special.sigma.AvlTree
  case class AvlTreeConst(
        constValue: SAvlTree
      ) extends AvlTree with LiftedConst[SAvlTree, AvlTree] {
    val liftable: Liftable[SAvlTree, AvlTree] = LiftableAvlTree
    val selfType: Elem[AvlTree] = liftable.eW
    def builder: Rep[SigmaDslBuilder] = delayInvoke
    def startingDigest: Rep[Col[Byte]] = delayInvoke
    def keyLength: Rep[Int] = delayInvoke
    def valueLengthOpt: Rep[WOption[Int]] = delayInvoke
    def maxNumOperations: Rep[WOption[Int]] = delayInvoke
    def maxDeletes: Rep[WOption[Int]] = delayInvoke
    def dataSize: Rep[Long] = delayInvoke
  }

  implicit object LiftableAvlTree
    extends Liftable[SAvlTree, AvlTree] {
    lazy val eW: Elem[AvlTree] = avlTreeElement
    lazy val sourceClassTag: ClassTag[SAvlTree] = {
      classTag[SAvlTree]
    }
    def lift(x: SAvlTree): Rep[AvlTree] = AvlTreeConst(x)
    def unlift(w: Rep[AvlTree]): SAvlTree = w match {
      case Def(AvlTreeConst(x: SAvlTree))
            => x.asInstanceOf[SAvlTree]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAvlTree(p: Rep[AvlTree]): AvlTree = {
    proxyOps[AvlTree](p)(scala.reflect.classTag[AvlTree])
  }

  // familyElem
  class AvlTreeElem[To <: AvlTree]
    extends DslObjectElem[To] {
    override val liftable = LiftableAvlTree.asLiftable[SAvlTree, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[AvlTree], classOf[SAvlTree], Set(
        "startingDigest", "keyLength", "valueLengthOpt", "maxNumOperations", "maxDeletes", "dataSize"
      ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslObjectElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[AvlTree].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[AvlTree] => convertAvlTree(x) }
      tryConvert(element[AvlTree], this, x, conv)
    }

    def convertAvlTree(x: Rep[AvlTree]): Rep[To] = {
      x.elem match {
        case _: AvlTreeElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have AvlTreeElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def avlTreeElement: Elem[AvlTree] =
    cachedElem[AvlTreeElem[AvlTree]]()

  implicit case object AvlTreeCompanionElem extends CompanionElem[AvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[AvlTreeCompanionCtor]
    protected def getDefaultRep = RAvlTree
  }

  abstract class AvlTreeCompanionCtor extends CompanionDef[AvlTreeCompanionCtor] with AvlTreeCompanion {
    def selfType = AvlTreeCompanionElem
    override def toString = "AvlTree"
  }
  implicit def proxyAvlTreeCompanionCtor(p: Rep[AvlTreeCompanionCtor]): AvlTreeCompanionCtor =
    proxyOps[AvlTreeCompanionCtor](p)

  lazy val RAvlTree: Rep[AvlTreeCompanionCtor] = new AvlTreeCompanionCtor {
  }

  object AvlTreeMethods {
    object startingDigest {
      def unapply(d: Def[_]): Option[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "startingDigest" =>
          Some(receiver).asInstanceOf[Option[Rep[AvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object keyLength {
      def unapply(d: Def[_]): Option[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "keyLength" =>
          Some(receiver).asInstanceOf[Option[Rep[AvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object valueLengthOpt {
      def unapply(d: Def[_]): Option[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "valueLengthOpt" =>
          Some(receiver).asInstanceOf[Option[Rep[AvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object maxNumOperations {
      def unapply(d: Def[_]): Option[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "maxNumOperations" =>
          Some(receiver).asInstanceOf[Option[Rep[AvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object maxDeletes {
      def unapply(d: Def[_]): Option[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "maxDeletes" =>
          Some(receiver).asInstanceOf[Option[Rep[AvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Option[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[AvlTree]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AvlTreeCompanionMethods {
  }
} // of object AvlTree
  registerEntityObject("AvlTree", AvlTree)

object Context extends EntityObject("Context") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SContext = special.sigma.Context
  case class ContextConst(
        constValue: SContext
      ) extends Context with LiftedConst[SContext, Context] {
    val liftable: Liftable[SContext, Context] = LiftableContext
    val selfType: Elem[Context] = liftable.eW
    def builder: Rep[SigmaDslBuilder] = delayInvoke
    def OUTPUTS: Rep[Col[Box]] = delayInvoke
    def INPUTS: Rep[Col[Box]] = delayInvoke
    def HEIGHT: Rep[Long] = delayInvoke
    def SELF: Rep[Box] = delayInvoke
    def LastBlockUtxoRootHash: Rep[AvlTree] = delayInvoke
    def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = delayInvoke
    def deserialize[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = delayInvoke
  }

  implicit object LiftableContext
    extends Liftable[SContext, Context] {
    lazy val eW: Elem[Context] = contextElement
    lazy val sourceClassTag: ClassTag[SContext] = {
      classTag[SContext]
    }
    def lift(x: SContext): Rep[Context] = ContextConst(x)
    def unlift(w: Rep[Context]): SContext = w match {
      case Def(ContextConst(x: SContext))
            => x.asInstanceOf[SContext]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyContext(p: Rep[Context]): Context = {
    proxyOps[Context](p)(scala.reflect.classTag[Context])
  }

  // familyElem
  class ContextElem[To <: Context]
    extends EntityElem[To] {
    override val liftable = LiftableContext.asLiftable[SContext, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[Context], classOf[SContext], Set(
        "builder", "OUTPUTS", "INPUTS", "HEIGHT", "SELF", "LastBlockUtxoRootHash", "getVar", "deserialize"
      ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Context].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Context] => convertContext(x) }
      tryConvert(element[Context], this, x, conv)
    }

    def convertContext(x: Rep[Context]): Rep[To] = {
      x.elem match {
        case _: ContextElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have ContextElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def contextElement: Elem[Context] =
    cachedElem[ContextElem[Context]]()

  implicit case object ContextCompanionElem extends CompanionElem[ContextCompanionCtor] {
    lazy val tag = weakTypeTag[ContextCompanionCtor]
    protected def getDefaultRep = RContext
  }

  abstract class ContextCompanionCtor extends CompanionDef[ContextCompanionCtor] with ContextCompanion {
    def selfType = ContextCompanionElem
    override def toString = "Context"
  }
  implicit def proxyContextCompanionCtor(p: Rep[ContextCompanionCtor]): ContextCompanionCtor =
    proxyOps[ContextCompanionCtor](p)

  lazy val RContext: Rep[ContextCompanionCtor] = new ContextCompanionCtor {
  }

  object ContextMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[Context]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Option[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "OUTPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[Context]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Option[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "INPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[Context]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Option[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "HEIGHT" =>
          Some(receiver).asInstanceOf[Option[Rep[Context]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Option[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "SELF" =>
          Some(receiver).asInstanceOf[Option[Rep[Context]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Option[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "LastBlockUtxoRootHash" =>
          Some(receiver).asInstanceOf[Option[Rep[Context]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Option[(Rep[Context], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(id, cT, emT, _*), _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "getVar" =>
          Some((receiver, id, cT, emT)).asInstanceOf[Option[(Rep[Context], Rep[Byte], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Context], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object deserialize {
      def unapply(d: Def[_]): Option[(Rep[Context], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(id, cT, emT, _*), _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "deserialize" =>
          Some((receiver, id, cT, emT)).asInstanceOf[Option[(Rep[Context], Rep[Byte], Elem[T], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Context], Rep[Byte], Elem[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ContextCompanionMethods {
  }
} // of object Context
  registerEntityObject("Context", Context)

object SigmaContract extends EntityObject("SigmaContract") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaContract = special.sigma.SigmaContract
  case class SigmaContractConst(
        constValue: SSigmaContract
      ) extends SigmaContract with LiftedConst[SSigmaContract, SigmaContract] {
    val liftable: Liftable[SSigmaContract, SigmaContract] = LiftableSigmaContract
    val selfType: Elem[SigmaContract] = liftable.eW
    def builder: Rep[SigmaDslBuilder] = delayInvoke
    @clause def canOpen(ctx: Rep[Context]): Rep[Boolean] = delayInvoke
  }

  implicit object LiftableSigmaContract
    extends Liftable[SSigmaContract, SigmaContract] {
    lazy val eW: Elem[SigmaContract] = sigmaContractElement
    lazy val sourceClassTag: ClassTag[SSigmaContract] = {
      classTag[SSigmaContract]
    }
    def lift(x: SSigmaContract): Rep[SigmaContract] = SigmaContractConst(x)
    def unlift(w: Rep[SigmaContract]): SSigmaContract = w match {
      case Def(SigmaContractConst(x: SSigmaContract))
            => x.asInstanceOf[SSigmaContract]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaContract(p: Rep[SigmaContract]): SigmaContract = {
    proxyOps[SigmaContract](p)(scala.reflect.classTag[SigmaContract])
  }

  // familyElem
  class SigmaContractElem[To <: SigmaContract]
    extends EntityElem[To] {
    override val liftable = LiftableSigmaContract.asLiftable[SSigmaContract, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[SigmaContract], classOf[SSigmaContract], Set(
        "builder", "Collection", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "proveDlog", "proveDHTuple", "isMember", "groupGenerator", "canOpen", "asFunction"
      ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaContract].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaContract] => convertSigmaContract(x) }
      tryConvert(element[SigmaContract], this, x, conv)
    }

    def convertSigmaContract(x: Rep[SigmaContract]): Rep[To] = {
      x.elem match {
        case _: SigmaContractElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SigmaContractElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaContractElement: Elem[SigmaContract] =
    cachedElem[SigmaContractElem[SigmaContract]]()

  implicit case object SigmaContractCompanionElem extends CompanionElem[SigmaContractCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaContractCompanionCtor]
    protected def getDefaultRep = RSigmaContract
  }

  abstract class SigmaContractCompanionCtor extends CompanionDef[SigmaContractCompanionCtor] with SigmaContractCompanion {
    def selfType = SigmaContractCompanionElem
    override def toString = "SigmaContract"
  }
  implicit def proxySigmaContractCompanionCtor(p: Rep[SigmaContractCompanionCtor]): SigmaContractCompanionCtor =
    proxyOps[SigmaContractCompanionCtor](p)

  lazy val RSigmaContract: Rep[SigmaContractCompanionCtor] = new SigmaContractCompanionCtor {
  }

  object SigmaContractMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaContract]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object Collection {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(items, emT, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "Collection" =>
          Some((receiver, items, emT)).asInstanceOf[Option[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(cond, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "verifyZK" =>
          Some((receiver, cond)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Int], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(bound, props, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "atLeast" =>
          Some((receiver, bound, props)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Int], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Int], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allZK" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyZK" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[String])] = d match {
        case MethodCall(receiver, method, Seq(base64String, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "PubKey" =>
          Some((receiver, base64String)).asInstanceOf[Option[(Rep[SigmaContract], Rep[String])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "sigmaProp" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "blake2b256" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "sha256" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "byteArrayToBigInt" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Long])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "longToByteArray" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Long])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(g, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "proveDlog" =>
          Some((receiver, g)).asInstanceOf[Option[(Rep[SigmaContract], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(g, h, u, v, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "proveDHTuple" =>
          Some((receiver, g, h, u, v)).asInstanceOf[Option[(Rep[SigmaContract], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isMember {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(tree, key, proof, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "isMember" =>
          Some((receiver, tree, key, proof)).asInstanceOf[Option[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Option[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "groupGenerator" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaContract]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object canOpen {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "canOpen" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object asFunction {
      def unapply(d: Def[_]): Option[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "asFunction" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaContract]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SigmaContractCompanionMethods {
  }
} // of object SigmaContract
  registerEntityObject("SigmaContract", SigmaContract)

object SigmaDslBuilder extends EntityObject("SigmaDslBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaDslBuilder = special.sigma.SigmaDslBuilder
  case class SigmaDslBuilderConst(
        constValue: SSigmaDslBuilder
      ) extends SigmaDslBuilder with LiftedConst[SSigmaDslBuilder, SigmaDslBuilder] {
    val liftable: Liftable[SSigmaDslBuilder, SigmaDslBuilder] = LiftableSigmaDslBuilder
    val selfType: Elem[SigmaDslBuilder] = liftable.eW
    def Cols: Rep[ColBuilder] = delayInvoke
    def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = delayInvoke
    def atLeast(bound: Rep[Int], props: Rep[Col[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = delayInvoke
    def allZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = delayInvoke
    def anyZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    def PubKey(base64String: Rep[String]): Rep[SigmaProp] = delayInvoke
    def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = delayInvoke
    def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = delayInvoke
    def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = delayInvoke
    def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger] = delayInvoke
    def longToByteArray(l: Rep[Long]): Rep[Col[Byte]] = delayInvoke
    def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp] = delayInvoke
    def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp] = delayInvoke
    def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean] = delayInvoke
    def groupGenerator: Rep[WECPoint] = delayInvoke
  }

  implicit object LiftableSigmaDslBuilder
    extends Liftable[SSigmaDslBuilder, SigmaDslBuilder] {
    lazy val eW: Elem[SigmaDslBuilder] = sigmaDslBuilderElement
    lazy val sourceClassTag: ClassTag[SSigmaDslBuilder] = {
      classTag[SSigmaDslBuilder]
    }
    def lift(x: SSigmaDslBuilder): Rep[SigmaDslBuilder] = SigmaDslBuilderConst(x)
    def unlift(w: Rep[SigmaDslBuilder]): SSigmaDslBuilder = w match {
      case Def(SigmaDslBuilderConst(x: SSigmaDslBuilder))
            => x.asInstanceOf[SSigmaDslBuilder]
      case _ => unliftError(w)
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaDslBuilder(p: Rep[SigmaDslBuilder]): SigmaDslBuilder = {
    proxyOps[SigmaDslBuilder](p)(scala.reflect.classTag[SigmaDslBuilder])
  }

  // familyElem
  class SigmaDslBuilderElem[To <: SigmaDslBuilder]
    extends DslBuilderElem[To] {
    override val liftable = LiftableSigmaDslBuilder.asLiftable[SSigmaDslBuilder, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++ Elem.declaredMethods(classOf[SigmaDslBuilder], classOf[SSigmaDslBuilder], Set(
        "Cols", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "proveDlog", "proveDHTuple", "isMember", "groupGenerator"
      ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaDslBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaDslBuilder] => convertSigmaDslBuilder(x) }
      tryConvert(element[SigmaDslBuilder], this, x, conv)
    }

    def convertSigmaDslBuilder(x: Rep[SigmaDslBuilder]): Rep[To] = {
      x.elem match {
        case _: SigmaDslBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SigmaDslBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaDslBuilderElement: Elem[SigmaDslBuilder] =
    cachedElem[SigmaDslBuilderElem[SigmaDslBuilder]]()

  implicit case object SigmaDslBuilderCompanionElem extends CompanionElem[SigmaDslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaDslBuilderCompanionCtor]
    protected def getDefaultRep = RSigmaDslBuilder
  }

  abstract class SigmaDslBuilderCompanionCtor extends CompanionDef[SigmaDslBuilderCompanionCtor] with SigmaDslBuilderCompanion {
    def selfType = SigmaDslBuilderCompanionElem
    override def toString = "SigmaDslBuilder"
  }
  implicit def proxySigmaDslBuilderCompanionCtor(p: Rep[SigmaDslBuilderCompanionCtor]): SigmaDslBuilderCompanionCtor =
    proxyOps[SigmaDslBuilderCompanionCtor](p)

  lazy val RSigmaDslBuilder: Rep[SigmaDslBuilderCompanionCtor] = new SigmaDslBuilderCompanionCtor {
  }

  object SigmaDslBuilderMethods {
    object Cols {
      def unapply(d: Def[_]): Option[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Cols" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaDslBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(cond, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "verifyZK" =>
          Some((receiver, cond)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(bound, props, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "atLeast" =>
          Some((receiver, bound, props)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "allOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "allZK" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "anyOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "anyZK" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[String])] = d match {
        case MethodCall(receiver, method, Seq(base64String, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "PubKey" =>
          Some((receiver, base64String)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[String])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "sigmaProp" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "blake2b256" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "sha256" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(bytes, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "byteArrayToBigInt" =>
          Some((receiver, bytes)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[Long])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "longToByteArray" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[Long])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(g, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "proveDlog" =>
          Some((receiver, g)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(g, h, u, v, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "proveDHTuple" =>
          Some((receiver, g, h, u, v)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isMember {
      def unapply(d: Def[_]): Option[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(tree, key, proof, _*), _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "isMember" =>
          Some((receiver, tree, key, proof)).asInstanceOf[Option[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Option[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "groupGenerator" =>
          Some(receiver).asInstanceOf[Option[Rep[SigmaDslBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SigmaDslBuilderCompanionMethods {
  }
} // of object SigmaDslBuilder
  registerEntityObject("SigmaDslBuilder", SigmaDslBuilder)

  registerModule(SigmaDslModule)
}

object SigmaDslModule extends scalan.ModuleInfo("special.sigma", "SigmaDsl")
}

trait SigmaDslModule extends special.sigma.impl.SigmaDslDefs {self: SigmaLibrary =>}
