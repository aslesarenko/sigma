package special.sigma.wrappers

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WrappersSpecDefs extends scalan.Scalan with WrappersSpec {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import WArray._
import WrapSpec._
import ECPointWrapSpec._
import SigmaPredefWrapSpec._

object ECPointWrapSpec extends EntityObject("ECPointWrapSpec") {
  case class ECPointWrapSpecCtor
      ()
    extends ECPointWrapSpec() with Def[ECPointWrapSpec] {
    lazy val selfType = element[ECPointWrapSpec]
  }
  // elem for concrete class
  class ECPointWrapSpecElem(val iso: Iso[ECPointWrapSpecData, ECPointWrapSpec])
    extends WrapSpecElem[ECPointWrapSpec]
    with ConcreteElem[ECPointWrapSpecData, ECPointWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = RECPointWrapSpec()
    override def getDefaultRep = RECPointWrapSpec()
    override lazy val tag = {
      weakTypeTag[ECPointWrapSpec]
    }
  }

  // state representation type
  type ECPointWrapSpecData = Unit

  // 3) Iso for concrete class
  class ECPointWrapSpecIso
    extends EntityIso[ECPointWrapSpecData, ECPointWrapSpec] with Def[ECPointWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[ECPointWrapSpec] => () }
    override def from(p: Rep[ECPointWrapSpec]) =
      tryConvert[ECPointWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RECPointWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new ECPointWrapSpecElem(self)
    lazy val selfType = new ECPointWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ECPointWrapSpecIsoElem() extends Elem[ECPointWrapSpecIso] {
    def getDefaultRep = reifyObject(new ECPointWrapSpecIso())
    lazy val tag = {
      weakTypeTag[ECPointWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ECPointWrapSpecCompanionCtor extends CompanionDef[ECPointWrapSpecCompanionCtor] with ECPointWrapSpecCompanion {
    def selfType = ECPointWrapSpecCompanionElem
    override def toString = "ECPointWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ECPointWrapSpecData]): Rep[ECPointWrapSpec] = {
      isoECPointWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[ECPointWrapSpec] =
      mkECPointWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkECPointWrapSpec(p)
  }
  lazy val ECPointWrapSpecRep: Rep[ECPointWrapSpecCompanionCtor] = new ECPointWrapSpecCompanionCtor
  lazy val RECPointWrapSpec: ECPointWrapSpecCompanionCtor = proxyECPointWrapSpecCompanion(ECPointWrapSpecRep)
  implicit def proxyECPointWrapSpecCompanion(p: Rep[ECPointWrapSpecCompanionCtor]): ECPointWrapSpecCompanionCtor = {
    proxyOps[ECPointWrapSpecCompanionCtor](p)
  }

  implicit case object ECPointWrapSpecCompanionElem extends CompanionElem[ECPointWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[ECPointWrapSpecCompanionCtor]
    protected def getDefaultRep = ECPointWrapSpecRep
  }

  implicit def proxyECPointWrapSpec(p: Rep[ECPointWrapSpec]): ECPointWrapSpec =
    proxyOps[ECPointWrapSpec](p)

  implicit class ExtendedECPointWrapSpec(p: Rep[ECPointWrapSpec]) {
    def toData: Rep[ECPointWrapSpecData] = {
      isoECPointWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoECPointWrapSpec: Iso[ECPointWrapSpecData, ECPointWrapSpec] =
    reifyObject(new ECPointWrapSpecIso())

  def mkECPointWrapSpec
    (): Rep[ECPointWrapSpec] = {
    new ECPointWrapSpecCtor()
  }
  def unmkECPointWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ECPointWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object ECPointWrapSpecMethods {
    object getEncoded {
      def unapply(d: Def[_]): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(g, emA, _*), _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "getEncoded" =>
          Some((receiver, g, emA)).asInstanceOf[Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Elem[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ECPointWrapSpecCompanionMethods {
  }
} // of object ECPointWrapSpec
  registerEntityObject("ECPointWrapSpec", ECPointWrapSpec)

object SigmaPredefWrapSpec extends EntityObject("SigmaPredefWrapSpec") {
  case class SigmaPredefWrapSpecCtor
      ()
    extends SigmaPredefWrapSpec() with Def[SigmaPredefWrapSpec] {
    lazy val selfType = element[SigmaPredefWrapSpec]
  }
  // elem for concrete class
  class SigmaPredefWrapSpecElem(val iso: Iso[SigmaPredefWrapSpecData, SigmaPredefWrapSpec])
    extends WrapSpecElem[SigmaPredefWrapSpec]
    with ConcreteElem[SigmaPredefWrapSpecData, SigmaPredefWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = RSigmaPredefWrapSpec()
    override def getDefaultRep = RSigmaPredefWrapSpec()
    override lazy val tag = {
      weakTypeTag[SigmaPredefWrapSpec]
    }
  }

  // state representation type
  type SigmaPredefWrapSpecData = Unit

  // 3) Iso for concrete class
  class SigmaPredefWrapSpecIso
    extends EntityIso[SigmaPredefWrapSpecData, SigmaPredefWrapSpec] with Def[SigmaPredefWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[SigmaPredefWrapSpec] => () }
    override def from(p: Rep[SigmaPredefWrapSpec]) =
      tryConvert[SigmaPredefWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RSigmaPredefWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new SigmaPredefWrapSpecElem(self)
    lazy val selfType = new SigmaPredefWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class SigmaPredefWrapSpecIsoElem() extends Elem[SigmaPredefWrapSpecIso] {
    def getDefaultRep = reifyObject(new SigmaPredefWrapSpecIso())
    lazy val tag = {
      weakTypeTag[SigmaPredefWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class SigmaPredefWrapSpecCompanionCtor extends CompanionDef[SigmaPredefWrapSpecCompanionCtor] with SigmaPredefWrapSpecCompanion {
    def selfType = SigmaPredefWrapSpecCompanionElem
    override def toString = "SigmaPredefWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[SigmaPredefWrapSpecData]): Rep[SigmaPredefWrapSpec] = {
      isoSigmaPredefWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[SigmaPredefWrapSpec] =
      mkSigmaPredefWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkSigmaPredefWrapSpec(p)
  }
  lazy val SigmaPredefWrapSpecRep: Rep[SigmaPredefWrapSpecCompanionCtor] = new SigmaPredefWrapSpecCompanionCtor
  lazy val RSigmaPredefWrapSpec: SigmaPredefWrapSpecCompanionCtor = proxySigmaPredefWrapSpecCompanion(SigmaPredefWrapSpecRep)
  implicit def proxySigmaPredefWrapSpecCompanion(p: Rep[SigmaPredefWrapSpecCompanionCtor]): SigmaPredefWrapSpecCompanionCtor = {
    proxyOps[SigmaPredefWrapSpecCompanionCtor](p)
  }

  implicit case object SigmaPredefWrapSpecCompanionElem extends CompanionElem[SigmaPredefWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaPredefWrapSpecCompanionCtor]
    protected def getDefaultRep = SigmaPredefWrapSpecRep
  }

  implicit def proxySigmaPredefWrapSpec(p: Rep[SigmaPredefWrapSpec]): SigmaPredefWrapSpec =
    proxyOps[SigmaPredefWrapSpec](p)

  implicit class ExtendedSigmaPredefWrapSpec(p: Rep[SigmaPredefWrapSpec]) {
    def toData: Rep[SigmaPredefWrapSpecData] = {
      isoSigmaPredefWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoSigmaPredefWrapSpec: Iso[SigmaPredefWrapSpecData, SigmaPredefWrapSpec] =
    reifyObject(new SigmaPredefWrapSpecIso())

  def mkSigmaPredefWrapSpec
    (): Rep[SigmaPredefWrapSpec] = {
    new SigmaPredefWrapSpecCtor()
  }
  def unmkSigmaPredefWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SigmaPredefWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object SigmaPredefWrapSpecMethods {
    object cost {
      def unapply(d: Def[_]): Option[(Rep[SigmaPredefWrapSpec], Rep[Any])] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[SigmaPredefWrapSpecElem] && method.getName == "cost" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[SigmaPredefWrapSpec], Rep[Any])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SigmaPredefWrapSpec], Rep[Any])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SigmaPredefWrapSpecCompanionMethods {
  }
} // of object SigmaPredefWrapSpec
  registerEntityObject("SigmaPredefWrapSpec", SigmaPredefWrapSpec)

  registerModule(WrappersSpecModule)
}

object WrappersSpecModule extends scalan.ModuleInfo("special.sigma.wrappers", "WrappersSpec")
}

trait WrappersSpecModule extends special.sigma.wrappers.impl.WrappersSpecDefs {self: SigmaLibrary =>}
