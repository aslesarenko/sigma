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
import WECPoint._
import WrapSpecBase._
import WBigInteger._
import ECPointWrapSpec._
import BigIntegerWrapSpec._
import SigmaPredefWrapSpec._

object ECPointWrapSpec extends EntityObject("ECPointWrapSpec") {
  case class ECPointWrapSpecCtor
      ()
    extends ECPointWrapSpec() with Def[ECPointWrapSpec] {
    lazy val selfType = element[ECPointWrapSpec]
  }
  // elem for concrete class
  class ECPointWrapSpecElem(val iso: Iso[ECPointWrapSpecData, ECPointWrapSpec])
    extends WrapSpecBaseElem[ECPointWrapSpec]
    with ConcreteElem[ECPointWrapSpecData, ECPointWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = RECPointWrapSpec()
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

    def unapply(p: Rep[WrapSpecBase]) = unmkECPointWrapSpec(p)
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
  def unmkECPointWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ECPointWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object ECPointWrapSpecMethods {
    object getEncoded {
      def unapply(d: Def[_]): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(g, compressed, emA, _*), _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "getEncoded" =>
          Some((receiver, g, compressed, emA)).asInstanceOf[Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "multiply" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "add" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ECPointWrapSpecCompanionMethods {
  }
} // of object ECPointWrapSpec
  registerEntityObject("ECPointWrapSpec", ECPointWrapSpec)

object BigIntegerWrapSpec extends EntityObject("BigIntegerWrapSpec") {
  case class BigIntegerWrapSpecCtor
      ()
    extends BigIntegerWrapSpec() with Def[BigIntegerWrapSpec] {
    lazy val selfType = element[BigIntegerWrapSpec]
  }
  // elem for concrete class
  class BigIntegerWrapSpecElem(val iso: Iso[BigIntegerWrapSpecData, BigIntegerWrapSpec])
    extends WrapSpecBaseElem[BigIntegerWrapSpec]
    with ConcreteElem[BigIntegerWrapSpecData, BigIntegerWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = RBigIntegerWrapSpec()
    override def getDefaultRep = RBigIntegerWrapSpec()
    override lazy val tag = {
      weakTypeTag[BigIntegerWrapSpec]
    }
  }

  // state representation type
  type BigIntegerWrapSpecData = Unit

  // 3) Iso for concrete class
  class BigIntegerWrapSpecIso
    extends EntityIso[BigIntegerWrapSpecData, BigIntegerWrapSpec] with Def[BigIntegerWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[BigIntegerWrapSpec] => () }
    override def from(p: Rep[BigIntegerWrapSpec]) =
      tryConvert[BigIntegerWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RBigIntegerWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new BigIntegerWrapSpecElem(self)
    lazy val selfType = new BigIntegerWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class BigIntegerWrapSpecIsoElem() extends Elem[BigIntegerWrapSpecIso] {
    def getDefaultRep = reifyObject(new BigIntegerWrapSpecIso())
    lazy val tag = {
      weakTypeTag[BigIntegerWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class BigIntegerWrapSpecCompanionCtor extends CompanionDef[BigIntegerWrapSpecCompanionCtor] with BigIntegerWrapSpecCompanion {
    def selfType = BigIntegerWrapSpecCompanionElem
    override def toString = "BigIntegerWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[BigIntegerWrapSpecData]): Rep[BigIntegerWrapSpec] = {
      isoBigIntegerWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[BigIntegerWrapSpec] =
      mkBigIntegerWrapSpec()

    def unapply(p: Rep[WrapSpecBase]) = unmkBigIntegerWrapSpec(p)
  }
  lazy val BigIntegerWrapSpecRep: Rep[BigIntegerWrapSpecCompanionCtor] = new BigIntegerWrapSpecCompanionCtor
  lazy val RBigIntegerWrapSpec: BigIntegerWrapSpecCompanionCtor = proxyBigIntegerWrapSpecCompanion(BigIntegerWrapSpecRep)
  implicit def proxyBigIntegerWrapSpecCompanion(p: Rep[BigIntegerWrapSpecCompanionCtor]): BigIntegerWrapSpecCompanionCtor = {
    proxyOps[BigIntegerWrapSpecCompanionCtor](p)
  }

  implicit case object BigIntegerWrapSpecCompanionElem extends CompanionElem[BigIntegerWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[BigIntegerWrapSpecCompanionCtor]
    protected def getDefaultRep = BigIntegerWrapSpecRep
  }

  implicit def proxyBigIntegerWrapSpec(p: Rep[BigIntegerWrapSpec]): BigIntegerWrapSpec =
    proxyOps[BigIntegerWrapSpec](p)

  implicit class ExtendedBigIntegerWrapSpec(p: Rep[BigIntegerWrapSpec]) {
    def toData: Rep[BigIntegerWrapSpecData] = {
      isoBigIntegerWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoBigIntegerWrapSpec: Iso[BigIntegerWrapSpecData, BigIntegerWrapSpec] =
    reifyObject(new BigIntegerWrapSpecIso())

  def mkBigIntegerWrapSpec
    (): Rep[BigIntegerWrapSpec] = {
    new BigIntegerWrapSpecCtor()
  }
  def unmkBigIntegerWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BigIntegerWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object BigIntegerWrapSpecMethods {
    object fromString {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[String])] = d match {
        case MethodCall(receiver, method, Seq(s, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "fromString" =>
          Some((receiver, s)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[String])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(sig, arr, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "fromArray" =>
          Some((receiver, sig, arr)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object ZERO {
      def unapply(d: Def[_]): Option[Rep[BigIntegerWrapSpec]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "ZERO" =>
          Some(receiver).asInstanceOf[Option[Rep[BigIntegerWrapSpec]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[BigIntegerWrapSpec]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object ONE {
      def unapply(d: Def[_]): Option[Rep[BigIntegerWrapSpec]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "ONE" =>
          Some(receiver).asInstanceOf[Option[Rep[BigIntegerWrapSpec]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[BigIntegerWrapSpec]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object valueOf {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[Long])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "valueOf" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[Long])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toStringWithRadix {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, radix, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "toStringWithRadix" =>
          Some((receiver, l, radix)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toByteArray {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "toByteArray" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "add" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "subtract" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "multiply" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mod {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "mod" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object modInverse {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "modInverse" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object modPow {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, exponent, m, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "modPow" =>
          Some((receiver, l, exponent, m)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "remainder" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object divide {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "divide" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "compareTo" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object min {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "min" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object max {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "max" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object gcd {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "gcd" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "and" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "or" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object xor {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "xor" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object not {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "not" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object andNot {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "andNot" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "pow" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object testBit {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "testBit" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object setBit {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "setBit" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object clearBit {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "clearBit" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flitBit {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "flitBit" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getLowestSetBit {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "getLowestSetBit" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bitCount {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "bitCount" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bitLength {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "bitLength" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isProbablyPrime {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "isProbablyPrime" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shiftLeft {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shiftLeft" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shiftRight {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(l, r, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shiftRight" =>
          Some((receiver, l, r)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object abs {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "abs" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object negate {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "negate" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object signum {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "signum" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteValue {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "byteValue" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shortValue {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shortValue" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object intValue {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "intValue" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longValue {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "longValue" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteValueExact {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "byteValueExact" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shortValueExact {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shortValueExact" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object intValueExact {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "intValueExact" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longValueExact {
      def unapply(d: Def[_]): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "longValueExact" =>
          Some((receiver, l)).asInstanceOf[Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BigIntegerWrapSpecCompanionMethods {
  }
} // of object BigIntegerWrapSpec
  registerEntityObject("BigIntegerWrapSpec", BigIntegerWrapSpec)

object SigmaPredefWrapSpec extends EntityObject("SigmaPredefWrapSpec") {
  case class SigmaPredefWrapSpecCtor
      ()
    extends SigmaPredefWrapSpec() with Def[SigmaPredefWrapSpec] {
    lazy val selfType = element[SigmaPredefWrapSpec]
  }
  // elem for concrete class
  class SigmaPredefWrapSpecElem(val iso: Iso[SigmaPredefWrapSpecData, SigmaPredefWrapSpec])
    extends WrapSpecBaseElem[SigmaPredefWrapSpec]
    with ConcreteElem[SigmaPredefWrapSpecData, SigmaPredefWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = RSigmaPredefWrapSpec()
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

    def unapply(p: Rep[WrapSpecBase]) = unmkSigmaPredefWrapSpec(p)
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
  def unmkSigmaPredefWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SigmaPredefWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object SigmaPredefWrapSpecMethods {
    object dataSize {
      def unapply(d: Def[_]): Option[(Rep[SigmaPredefWrapSpec], Rep[Any])] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[SigmaPredefWrapSpecElem] && method.getName == "dataSize" =>
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
