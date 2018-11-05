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
import WBigInteger._
import WECPoint._
import WrapSpecBase._
import BigIntegerWrapSpec._
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
    if (p.rhs.isInstanceOf[ECPointWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[ECPointWrapSpecCompanionCtor]
    else
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
      def unapply(d: Def[_]): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "getEncoded" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "multiply" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem] && method.getName == "add" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
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
    if (p.rhs.isInstanceOf[BigIntegerWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[BigIntegerWrapSpecCompanionCtor]
    else
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
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "fromString" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "fromArray" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object ZERO {
      def unapply(d: Def[_]): Nullable[Rep[BigIntegerWrapSpec]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "ZERO" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigIntegerWrapSpec]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigIntegerWrapSpec]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object ONE {
      def unapply(d: Def[_]): Nullable[Rep[BigIntegerWrapSpec]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "ONE" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigIntegerWrapSpec]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigIntegerWrapSpec]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueOf {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "valueOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // manual fix (method name)
    object toStringMethod {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "toString" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "toByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "add" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "subtract" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "multiply" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "mod" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modInverse {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "modInverse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modPow {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "modPow" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "remainder" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "divide" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "compareTo" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "min" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "max" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object gcd {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "gcd" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "and" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "or" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "xor" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object not {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "not" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object andNot {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "andNot" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object pow {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "pow" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object testBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "testBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object setBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "setBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object clearBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "clearBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flipBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "flipBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getLowestSetBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "getLowestSetBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitCount {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "bitCount" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitLength {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "bitLength" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isProbablePrime {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "isProbablePrime" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftLeft {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shiftLeft" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftRight {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shiftRight" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object abs {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "abs" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object negate {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "negate" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object signum {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "signum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "byteValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shortValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "intValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "longValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "byteValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "shortValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "intValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem] && method.getName == "longValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
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
    if (p.rhs.isInstanceOf[SigmaPredefWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[SigmaPredefWrapSpecCompanionCtor]
    else
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
      def unapply(d: Def[_]): Nullable[(Rep[SigmaPredefWrapSpec], Rep[Any])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPredefWrapSpecElem] && method.getName == "dataSize" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaPredefWrapSpec], Rep[Any])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaPredefWrapSpec], Rep[Any])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
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
