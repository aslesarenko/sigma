package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslDefs extends scalan.Scalan with SigmaDsl {
  self: SigmaLibrary =>

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
    protected def getDefaultRep = Sigma
  }

  abstract class SigmaCompanionCtor extends CompanionDef[SigmaCompanionCtor] with SigmaCompanion {
    def selfType = SigmaCompanionElem
    override def toString = "Sigma"
  }
  implicit def proxySigmaCompanionCtor(p: Rep[SigmaCompanionCtor]): SigmaCompanionCtor =
    proxyOps[SigmaCompanionCtor](p)

  lazy val Sigma: Rep[SigmaCompanionCtor] = new SigmaCompanionCtor {
  }

  object SigmaMethods {
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
    protected def getDefaultRep = ProveDlog
  }

  abstract class ProveDlogCompanionCtor extends CompanionDef[ProveDlogCompanionCtor] with ProveDlogCompanion {
    def selfType = ProveDlogCompanionElem
    override def toString = "ProveDlog"
  }
  implicit def proxyProveDlogCompanionCtor(p: Rep[ProveDlogCompanionCtor]): ProveDlogCompanionCtor =
    proxyOps[ProveDlogCompanionCtor](p)

  lazy val ProveDlog: Rep[ProveDlogCompanionCtor] = new ProveDlogCompanionCtor {
  }

  object ProveDlogMethods {
    object propBytes {
      def unapply(d: Def[_]): Option[Rep[ProveDlog]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogElem[_]] && method.getName == "propBytes" =>
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
    protected def getDefaultRep = Box
  }

  abstract class BoxCompanionCtor extends CompanionDef[BoxCompanionCtor] with BoxCompanion {
    def selfType = BoxCompanionElem
    override def toString = "Box"
  }
  implicit def proxyBoxCompanionCtor(p: Rep[BoxCompanionCtor]): BoxCompanionCtor =
    proxyOps[BoxCompanionCtor](p)

  lazy val Box: Rep[BoxCompanionCtor] = new BoxCompanionCtor {
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
    protected def getDefaultRep = Context
  }

  abstract class ContextCompanionCtor extends CompanionDef[ContextCompanionCtor] with ContextCompanion {
    def selfType = ContextCompanionElem
    override def toString = "Context"
  }
  implicit def proxyContextCompanionCtor(p: Rep[ContextCompanionCtor]): ContextCompanionCtor =
    proxyOps[ContextCompanionCtor](p)

  lazy val Context: Rep[ContextCompanionCtor] = new ContextCompanionCtor {
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
  }

  object ContextCompanionMethods {
  }

  // entityProxy: single proxy for each type family
  implicit def proxyContextBuilder(p: Rep[ContextBuilder]): ContextBuilder = {
    proxyOps[ContextBuilder](p)(scala.reflect.classTag[ContextBuilder])
  }

  // familyElem
  class ContextBuilderElem[To <: ContextBuilder]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
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
    protected def getDefaultRep = ContextBuilder
  }

  abstract class ContextBuilderCompanionCtor extends CompanionDef[ContextBuilderCompanionCtor] with ContextBuilderCompanion {
    def selfType = ContextBuilderCompanionElem
    override def toString = "ContextBuilder"
  }
  implicit def proxyContextBuilderCompanionCtor(p: Rep[ContextBuilderCompanionCtor]): ContextBuilderCompanionCtor =
    proxyOps[ContextBuilderCompanionCtor](p)

  lazy val ContextBuilder: Rep[ContextBuilderCompanionCtor] = new ContextBuilderCompanionCtor {
  }

  object ContextBuilderMethods {
    object Collections {
      def unapply(d: Def[_]): Option[Rep[ContextBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextBuilderElem[_]] && method.getName == "Collections" =>
          Some(receiver).asInstanceOf[Option[Rep[ContextBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ContextBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ContextBuilderCompanionMethods {
  }

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
    protected def getDefaultRep = SigmaContract
  }

  abstract class SigmaContractCompanionCtor extends CompanionDef[SigmaContractCompanionCtor] with SigmaContractCompanion {
    def selfType = SigmaContractCompanionElem
    override def toString = "SigmaContract"
  }
  implicit def proxySigmaContractCompanionCtor(p: Rep[SigmaContractCompanionCtor]): SigmaContractCompanionCtor =
    proxyOps[SigmaContractCompanionCtor](p)

  lazy val SigmaContract: Rep[SigmaContractCompanionCtor] = new SigmaContractCompanionCtor {
  }

  object SigmaContractMethods {
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
      def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[Context], Rep[Box])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _SELF, _*), _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "canOpen" =>
          Some((receiver, ctx, _SELF)).asInstanceOf[Option[(Rep[SigmaContract], Rep[Context], Rep[Box])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaContract], Rep[Context], Rep[Box])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SigmaContractCompanionMethods {
  }

  registerModule(SigmaDslModule)
}

object SigmaDslModule extends scalan.ModuleInfo("special.sigma", "SigmaDsl")
}

trait SigmaDslModule extends special.sigma.impl.SigmaDslDefs {self: SigmaLibrary =>}
