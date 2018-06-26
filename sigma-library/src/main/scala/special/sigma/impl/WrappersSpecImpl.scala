package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WrappersSpecDefs extends scalan.Scalan with WrappersSpec {
  self: SigmaLibrary =>

  case class OptionWrapSpecCtor
      ()
    extends OptionWrapSpec() with Def[OptionWrapSpec] {
    lazy val selfType = element[OptionWrapSpec]
  }
  // elem for concrete class
  class OptionWrapSpecElem(val iso: Iso[OptionWrapSpecData, OptionWrapSpec])
    extends WrapSpecElem[OptionWrapSpec]
    with ConcreteElem[OptionWrapSpecData, OptionWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = OptionWrapSpec()
    override def getDefaultRep = OptionWrapSpec()
    override lazy val tag = {
      weakTypeTag[OptionWrapSpec]
    }
  }

  // state representation type
  type OptionWrapSpecData = Unit

  // 3) Iso for concrete class
  class OptionWrapSpecIso
    extends EntityIso[OptionWrapSpecData, OptionWrapSpec] with Def[OptionWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[OptionWrapSpec] => () }
    override def from(p: Rep[OptionWrapSpec]) =
      tryConvert[OptionWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      OptionWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new OptionWrapSpecElem(self)
    lazy val selfType = new OptionWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class OptionWrapSpecIsoElem() extends Elem[OptionWrapSpecIso] {
    def getDefaultRep = reifyObject(new OptionWrapSpecIso())
    lazy val tag = {
      weakTypeTag[OptionWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class OptionWrapSpecCompanionCtor extends CompanionDef[OptionWrapSpecCompanionCtor] with OptionWrapSpecCompanion {
    def selfType = OptionWrapSpecCompanionElem
    override def toString = "OptionWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[OptionWrapSpecData]): Rep[OptionWrapSpec] = {
      isoOptionWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[OptionWrapSpec] =
      mkOptionWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkOptionWrapSpec(p)
  }
  lazy val OptionWrapSpecRep: Rep[OptionWrapSpecCompanionCtor] = new OptionWrapSpecCompanionCtor
  lazy val OptionWrapSpec: OptionWrapSpecCompanionCtor = proxyOptionWrapSpecCompanion(OptionWrapSpecRep)
  implicit def proxyOptionWrapSpecCompanion(p: Rep[OptionWrapSpecCompanionCtor]): OptionWrapSpecCompanionCtor = {
    proxyOps[OptionWrapSpecCompanionCtor](p)
  }

  implicit case object OptionWrapSpecCompanionElem extends CompanionElem[OptionWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[OptionWrapSpecCompanionCtor]
    protected def getDefaultRep = OptionWrapSpecRep
  }

  implicit def proxyOptionWrapSpec(p: Rep[OptionWrapSpec]): OptionWrapSpec =
    proxyOps[OptionWrapSpec](p)

  implicit class ExtendedOptionWrapSpec(p: Rep[OptionWrapSpec]) {
    def toData: Rep[OptionWrapSpecData] = {
      isoOptionWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoOptionWrapSpec: Iso[OptionWrapSpecData, OptionWrapSpec] =
    reifyObject(new OptionWrapSpecIso())

  registerModule(WrappersSpecModule)

  object OptionWrapSpecMethods {
    object get {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "get" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, f, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "map" =>
          Some((receiver, xs, f)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object OptionWrapSpecCompanionMethods {
  }

  def mkOptionWrapSpec
    (): Rep[OptionWrapSpec] = {
    new OptionWrapSpecCtor()
  }
  def unmkOptionWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: OptionWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }
}

object WrappersSpecModule extends scalan.ModuleInfo("special.sigma", "WrappersSpec")
}

trait WrappersSpecModule extends special.sigma.impl.WrappersSpecDefs {self: SigmaLibrary =>}
