package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslDefs extends scalan.Scalan with SigmaDsl {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import DslBuilder._
import SigmaDslBuilder._
import Col._
import Sigma._
import WECPoint._
import AnyValue._
import BoxBuilder._
import WOption._
import Box._
import ContextBuilder._
import Context._
import SigmaContractBuilder._
import SigmaContract._
import ColBuilder._
import SigmaBuilder._
import ProveDlog._

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

object Sigma extends EntityObject("Sigma") {
  // entityProxy: single proxy for each type family
  implicit def proxySigma(p: Rep[Sigma]): Sigma = {
    proxyOps[Sigma](p)(scala.reflect.classTag[Sigma])
  }

  // familyElem
  class SigmaElem[To <: Sigma]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Sigma].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Sigma] => convertSigma(x) }
      tryConvert(element[Sigma], this, x, conv)
    }

    def convertSigma(x: Rep[Sigma]): Rep[To] = {
      x.elem match {
        case _: SigmaElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SigmaElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaElement: Elem[Sigma] =
    cachedElem[SigmaElem[Sigma]]()

  implicit case object SigmaCompanionElem extends CompanionElem[SigmaCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaCompanionCtor]
    protected def getDefaultRep = RSigma
  }

  abstract class SigmaCompanionCtor extends CompanionDef[SigmaCompanionCtor] with SigmaCompanion {
    def selfType = SigmaCompanionElem
    override def toString = "Sigma"
  }
  implicit def proxySigmaCompanionCtor(p: Rep[SigmaCompanionCtor]): SigmaCompanionCtor =
    proxyOps[SigmaCompanionCtor](p)

  lazy val RSigma: Rep[SigmaCompanionCtor] = new SigmaCompanionCtor {
  }

  object SigmaMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[Sigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[Sigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Sigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isValid {
      def unapply(d: Def[_]): Option[Rep[Sigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "isValid" =>
          Some(receiver).asInstanceOf[Option[Rep[Sigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Sigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object propBytes {
      def unapply(d: Def[_]): Option[Rep[Sigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[Sigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Sigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Option[(Rep[Sigma], Rep[Sigma])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[Sigma], Rep[Sigma])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Sigma], Rep[Sigma])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Option[(Rep[Sigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[Sigma], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Sigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Option[(Rep[Sigma], Rep[Sigma])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[Sigma], Rep[Sigma])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Sigma], Rep[Sigma])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Option[(Rep[Sigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[SigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[Sigma], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Sigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SigmaCompanionMethods {
  }
} // of object Sigma
  registerEntityObject("Sigma", Sigma)

object SigmaBuilder extends EntityObject("SigmaBuilder") {
  // entityProxy: single proxy for each type family
  implicit def proxySigmaBuilder(p: Rep[SigmaBuilder]): SigmaBuilder = {
    proxyOps[SigmaBuilder](p)(scala.reflect.classTag[SigmaBuilder])
  }

  // familyElem
  class SigmaBuilderElem[To <: SigmaBuilder]
    extends DslBuilderElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(dslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaBuilder] => convertSigmaBuilder(x) }
      tryConvert(element[SigmaBuilder], this, x, conv)
    }

    def convertSigmaBuilder(x: Rep[SigmaBuilder]): Rep[To] = {
      x.elem match {
        case _: SigmaBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SigmaBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaBuilderElement: Elem[SigmaBuilder] =
    cachedElem[SigmaBuilderElem[SigmaBuilder]]()

  implicit case object SigmaBuilderCompanionElem extends CompanionElem[SigmaBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaBuilderCompanionCtor]
    protected def getDefaultRep = RSigmaBuilder
  }

  abstract class SigmaBuilderCompanionCtor extends CompanionDef[SigmaBuilderCompanionCtor] with SigmaBuilderCompanion {
    def selfType = SigmaBuilderCompanionElem
    override def toString = "SigmaBuilder"
  }
  implicit def proxySigmaBuilderCompanionCtor(p: Rep[SigmaBuilderCompanionCtor]): SigmaBuilderCompanionCtor =
    proxyOps[SigmaBuilderCompanionCtor](p)

  lazy val RSigmaBuilder: Rep[SigmaBuilderCompanionCtor] = new SigmaBuilderCompanionCtor {
  }

  object SigmaBuilderMethods {
  }

  object SigmaBuilderCompanionMethods {
  }
} // of object SigmaBuilder
  registerEntityObject("SigmaBuilder", SigmaBuilder)

object ProveDlog extends EntityObject("ProveDlog") {
  // entityProxy: single proxy for each type family
  implicit def proxyProveDlog(p: Rep[ProveDlog]): ProveDlog = {
    proxyOps[ProveDlog](p)(scala.reflect.classTag[ProveDlog])
  }

  // familyElem
  class ProveDlogElem[To <: ProveDlog]
    extends SigmaElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[ProveDlog].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ProveDlog] => convertProveDlog(x) }
      tryConvert(element[ProveDlog], this, x, conv)
    }

    def convertProveDlog(x: Rep[ProveDlog]): Rep[To] = {
      x.elem match {
        case _: ProveDlogElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have ProveDlogElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def proveDlogElement: Elem[ProveDlog] =
    cachedElem[ProveDlogElem[ProveDlog]]()

  implicit case object ProveDlogCompanionElem extends CompanionElem[ProveDlogCompanionCtor] {
    lazy val tag = weakTypeTag[ProveDlogCompanionCtor]
    protected def getDefaultRep = RProveDlog
  }

  abstract class ProveDlogCompanionCtor extends CompanionDef[ProveDlogCompanionCtor] with ProveDlogCompanion {
    def selfType = ProveDlogCompanionElem
    override def toString = "ProveDlog"
  }
  implicit def proxyProveDlogCompanionCtor(p: Rep[ProveDlogCompanionCtor]): ProveDlogCompanionCtor =
    proxyOps[ProveDlogCompanionCtor](p)

  lazy val RProveDlog: Rep[ProveDlogCompanionCtor] = new ProveDlogCompanionCtor {
  }

  object ProveDlogMethods {
    object value {
      def unapply(d: Def[_]): Option[Rep[ProveDlog]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[ProveDlog]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ProveDlog]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ProveDlogCompanionMethods {
  }
} // of object ProveDlog
  registerEntityObject("ProveDlog", ProveDlog)

object AnyValue extends EntityObject("AnyValue") {
  // entityProxy: single proxy for each type family
  implicit def proxyAnyValue(p: Rep[AnyValue]): AnyValue = {
    proxyOps[AnyValue](p)(scala.reflect.classTag[AnyValue])
  }

  // familyElem
  class AnyValueElem[To <: AnyValue]
    extends EntityElem[To] {
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
    object cost {
      def unapply(d: Def[_]): Option[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AnyValueElem[_]] && method.getName == "cost" =>
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
  // entityProxy: single proxy for each type family
  implicit def proxyBox(p: Rep[Box]): Box = {
    proxyOps[Box](p)(scala.reflect.classTag[Box])
  }

  // familyElem
  class BoxElem[To <: Box]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
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
    object builder {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[Box]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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

    object cost {
      def unapply(d: Def[_]): Option[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "cost" =>
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

    object getReg {
      def unapply(d: Def[_]): Option[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "getReg" =>
          Some((receiver, i, emT)).asInstanceOf[Option[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R0 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R0" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R1 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R1" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R2 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R2" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R3 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R3" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R4 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R4" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R5 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R5" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R6 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R6" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R7 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R7" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R8 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R8" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object R9 {
      def unapply(d: Def[_]): Option[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(emT, _*), _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R9" =>
          Some((receiver, emT)).asInstanceOf[Option[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BoxCompanionMethods {
  }
} // of object Box
  registerEntityObject("Box", Box)

object BoxBuilder extends EntityObject("BoxBuilder") {
  // entityProxy: single proxy for each type family
  implicit def proxyBoxBuilder(p: Rep[BoxBuilder]): BoxBuilder = {
    proxyOps[BoxBuilder](p)(scala.reflect.classTag[BoxBuilder])
  }

  // familyElem
  class BoxBuilderElem[To <: BoxBuilder]
    extends DslBuilderElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(dslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[BoxBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[BoxBuilder] => convertBoxBuilder(x) }
      tryConvert(element[BoxBuilder], this, x, conv)
    }

    def convertBoxBuilder(x: Rep[BoxBuilder]): Rep[To] = {
      x.elem match {
        case _: BoxBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have BoxBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def boxBuilderElement: Elem[BoxBuilder] =
    cachedElem[BoxBuilderElem[BoxBuilder]]()

  implicit case object BoxBuilderCompanionElem extends CompanionElem[BoxBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[BoxBuilderCompanionCtor]
    protected def getDefaultRep = RBoxBuilder
  }

  abstract class BoxBuilderCompanionCtor extends CompanionDef[BoxBuilderCompanionCtor] with BoxBuilderCompanion {
    def selfType = BoxBuilderCompanionElem
    override def toString = "BoxBuilder"
  }
  implicit def proxyBoxBuilderCompanionCtor(p: Rep[BoxBuilderCompanionCtor]): BoxBuilderCompanionCtor =
    proxyOps[BoxBuilderCompanionCtor](p)

  lazy val RBoxBuilder: Rep[BoxBuilderCompanionCtor] = new BoxBuilderCompanionCtor {
  }

  object BoxBuilderMethods {
  }

  object BoxBuilderCompanionMethods {
  }
} // of object BoxBuilder
  registerEntityObject("BoxBuilder", BoxBuilder)

object Context extends EntityObject("Context") {
  // entityProxy: single proxy for each type family
  implicit def proxyContext(p: Rep[Context]): Context = {
    proxyOps[Context](p)(scala.reflect.classTag[Context])
  }

  // familyElem
  class ContextElem[To <: Context]
    extends EntityElem[To] {
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

    object getVar {
      def unapply(d: Def[_]): Option[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(id, emT, _*), _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "getVar" =>
          Some((receiver, id, emT)).asInstanceOf[Option[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ContextCompanionMethods {
  }
} // of object Context
  registerEntityObject("Context", Context)

object ContextBuilder extends EntityObject("ContextBuilder") {
  // entityProxy: single proxy for each type family
  implicit def proxyContextBuilder(p: Rep[ContextBuilder]): ContextBuilder = {
    proxyOps[ContextBuilder](p)(scala.reflect.classTag[ContextBuilder])
  }

  // familyElem
  class ContextBuilderElem[To <: ContextBuilder]
    extends DslBuilderElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(dslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[ContextBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ContextBuilder] => convertContextBuilder(x) }
      tryConvert(element[ContextBuilder], this, x, conv)
    }

    def convertContextBuilder(x: Rep[ContextBuilder]): Rep[To] = {
      x.elem match {
        case _: ContextBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have ContextBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def contextBuilderElement: Elem[ContextBuilder] =
    cachedElem[ContextBuilderElem[ContextBuilder]]()

  implicit case object ContextBuilderCompanionElem extends CompanionElem[ContextBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[ContextBuilderCompanionCtor]
    protected def getDefaultRep = RContextBuilder
  }

  abstract class ContextBuilderCompanionCtor extends CompanionDef[ContextBuilderCompanionCtor] with ContextBuilderCompanion {
    def selfType = ContextBuilderCompanionElem
    override def toString = "ContextBuilder"
  }
  implicit def proxyContextBuilderCompanionCtor(p: Rep[ContextBuilderCompanionCtor]): ContextBuilderCompanionCtor =
    proxyOps[ContextBuilderCompanionCtor](p)

  lazy val RContextBuilder: Rep[ContextBuilderCompanionCtor] = new ContextBuilderCompanionCtor {
  }

  object ContextBuilderMethods {
  }

  object ContextBuilderCompanionMethods {
  }
} // of object ContextBuilder
  registerEntityObject("ContextBuilder", ContextBuilder)

object SigmaContract extends EntityObject("SigmaContract") {
  // entityProxy: single proxy for each type family
  implicit def proxySigmaContract(p: Rep[SigmaContract]): SigmaContract = {
    proxyOps[SigmaContract](p)(scala.reflect.classTag[SigmaContract])
  }

  // familyElem
  class SigmaContractElem[To <: SigmaContract]
    extends EntityElem[To] {
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

    object verify {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(cond, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "verify" =>
          Some((receiver, cond)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Sigma])] = d match {
        case MethodCall(receiver, method, Seq(cond, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "verifyZK" =>
          Some((receiver, cond)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Sigma])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Sigma])] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Sigma]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allZK" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Sigma]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Sigma]])] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Col[Sigma]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyZK" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Col[Sigma]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Col[Sigma]])] = exp match {
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
  }

  object SigmaContractCompanionMethods {
  }
} // of object SigmaContract
  registerEntityObject("SigmaContract", SigmaContract)

object SigmaContractBuilder extends EntityObject("SigmaContractBuilder") {
  // entityProxy: single proxy for each type family
  implicit def proxySigmaContractBuilder(p: Rep[SigmaContractBuilder]): SigmaContractBuilder = {
    proxyOps[SigmaContractBuilder](p)(scala.reflect.classTag[SigmaContractBuilder])
  }

  // familyElem
  class SigmaContractBuilderElem[To <: SigmaContractBuilder]
    extends DslBuilderElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(dslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaContractBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaContractBuilder] => convertSigmaContractBuilder(x) }
      tryConvert(element[SigmaContractBuilder], this, x, conv)
    }

    def convertSigmaContractBuilder(x: Rep[SigmaContractBuilder]): Rep[To] = {
      x.elem match {
        case _: SigmaContractBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SigmaContractBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaContractBuilderElement: Elem[SigmaContractBuilder] =
    cachedElem[SigmaContractBuilderElem[SigmaContractBuilder]]()

  implicit case object SigmaContractBuilderCompanionElem extends CompanionElem[SigmaContractBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaContractBuilderCompanionCtor]
    protected def getDefaultRep = RSigmaContractBuilder
  }

  abstract class SigmaContractBuilderCompanionCtor extends CompanionDef[SigmaContractBuilderCompanionCtor] with SigmaContractBuilderCompanion {
    def selfType = SigmaContractBuilderCompanionElem
    override def toString = "SigmaContractBuilder"
  }
  implicit def proxySigmaContractBuilderCompanionCtor(p: Rep[SigmaContractBuilderCompanionCtor]): SigmaContractBuilderCompanionCtor =
    proxyOps[SigmaContractBuilderCompanionCtor](p)

  lazy val RSigmaContractBuilder: Rep[SigmaContractBuilderCompanionCtor] = new SigmaContractBuilderCompanionCtor {
  }

  object SigmaContractBuilderMethods {
  }

  object SigmaContractBuilderCompanionMethods {
  }
} // of object SigmaContractBuilder
  registerEntityObject("SigmaContractBuilder", SigmaContractBuilder)

object SigmaDslBuilder extends EntityObject("SigmaDslBuilder") {
  // entityProxy: single proxy for each type family
  implicit def proxySigmaDslBuilder(p: Rep[SigmaDslBuilder]): SigmaDslBuilder = {
    proxyOps[SigmaDslBuilder](p)(scala.reflect.classTag[SigmaDslBuilder])
  }

  // familyElem
  class SigmaDslBuilderElem[To <: SigmaDslBuilder]
    extends SigmaBuilderElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaBuilderElement)
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
