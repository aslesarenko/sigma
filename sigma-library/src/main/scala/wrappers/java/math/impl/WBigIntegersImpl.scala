package wrappers.java.math

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import java.lang.reflect.Method  // manual fix
  import java.math.BigInteger  // manual fix

  import org.bouncycastle.math.ec.ECPoint  // manual fix
  import special.sigma.wrappers.BigIntegerWrapSpec  // manual fix
  import special.wrappers.ArrayWrapSpec  // manual fix

  // Abs -----------------------------------
trait WBigIntegersDefs extends scalan.Scalan with WBigIntegers {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WBigInteger._
import WArray._

object WBigInteger extends EntityObject("WBigInteger") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WBigIntegerConst(
        constValue: BigInteger
      ) extends WBigInteger with LiftedConst[BigInteger, WBigInteger] {
    val liftable: Liftable[BigInteger, WBigInteger] = LiftableBigInteger
    val selfType: Elem[WBigInteger] = liftable.eW
    private val thisClass = classOf[WBigInteger]

    def longValueExact: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("longValueExact"),
        List(),
        true, element[Long]))
    }

    def intValueExact: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("intValueExact"),
        List(),
        true, element[Int]))
    }

    def shortValueExact: Rep[Short] = {
      asRep[Short](mkMethodCall(self,
        thisClass.getMethod("shortValueExact"),
        List(),
        true, element[Short]))
    }

    def byteValueExact: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        thisClass.getMethod("byteValueExact"),
        List(),
        true, element[Byte]))
    }

    def longValue: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("longValue"),
        List(),
        true, element[Long]))
    }

    def intValue: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("intValue"),
        List(),
        true, element[Int]))
    }

    def shortValue: Rep[Short] = {
      asRep[Short](mkMethodCall(self,
        thisClass.getMethod("shortValue"),
        List(),
        true, element[Short]))
    }

    def byteValue: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        thisClass.getMethod("byteValue"),
        List(),
        true, element[Byte]))
    }

    def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("signum"),
        List(),
        true, element[Int]))
    }

    def negate: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("negate"),
        List(),
        true, element[WBigInteger]))
    }

    def abs: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("abs"),
        List(),
        true, element[WBigInteger]))
    }

    def shiftRight(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("shiftRight", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def shiftLeft(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("shiftLeft", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def isProbablePrime(x$1: Rep[Int]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isProbablePrime", classOf[Sym]),
        List(x$1),
        true, element[Boolean]))
    }

    def bitLength: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("bitLength"),
        List(),
        true, element[Int]))
    }

    def bitCount: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("bitCount"),
        List(),
        true, element[Int]))
    }

    def getLowestSetBit: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("getLowestSetBit"),
        List(),
        true, element[Int]))
    }

    def flipBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("flipBit", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def clearBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("clearBit", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def setBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("setBit", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def testBit(x$1: Rep[Int]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("testBit", classOf[Sym]),
        List(x$1),
        true, element[Boolean]))
    }

    def pow(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("pow", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def andNot(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("andNot", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def not: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("not"),
        List(),
        true, element[WBigInteger]))
    }

    def xor(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("xor", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def or(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("or", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def and(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("and", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def gcd(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("gcd", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def max(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("max", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def min(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("min", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def compareTo(x$1: Rep[WBigInteger]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("compareTo", classOf[Sym]),
        List(x$1),
        true, element[Int]))
    }

    def divide(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("divide", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def remainder(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("remainder", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def modPow(x$1: Rep[WBigInteger], x$2: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("modPow", classOf[Sym], classOf[Sym]),
        List(x$1, x$2),
        true, element[WBigInteger]))
    }

    def modInverse(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("modInverse", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def mod(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("mod", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def multiply(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("multiply", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def subtract(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("subtract", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def add(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("add", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def toByteArray: Rep[WArray[Byte]] = {
      asRep[WArray[Byte]](mkMethodCall(self,
        thisClass.getMethod("toByteArray"),
        List(),
        true, element[WArray[Byte]]))
    }

    def toString(x$1: Rep[Int]): Rep[String] = {
      asRep[String](mkMethodCall(self,
        thisClass.getMethod("toString", classOf[Sym]),
        List(x$1),
        true, element[String]))
    }
  }

  implicit object LiftableBigInteger
    extends Liftable[BigInteger, WBigInteger] {
    lazy val eW: Elem[WBigInteger] = wBigIntegerElement
    lazy val sourceClassTag: ClassTag[BigInteger] = {
      classTag[BigInteger]
    }
    def lift(x: BigInteger): Rep[WBigInteger] = WBigIntegerConst(x)
    def unlift(w: Rep[WBigInteger]): BigInteger = w match {
      case Def(WBigIntegerConst(x: BigInteger))
            => x.asInstanceOf[BigInteger]
      case _ => unliftError(w)
    }
  }

  private val _BigIntegerWrapSpec = new BigIntegerWrapSpec
  // entityProxy: single proxy for each type family
  implicit def proxyWBigInteger(p: Rep[WBigInteger]): WBigInteger = {
    if (p.rhs.isInstanceOf[WBigInteger@unchecked]) p.rhs.asInstanceOf[WBigInteger]
    else
      proxyOps[WBigInteger](p)(scala.reflect.classTag[WBigInteger])
  }

  // familyElem
  class WBigIntegerElem[To <: WBigInteger]
    extends EntityElem[To] {
    override val liftable = LiftableBigInteger.asLiftable[BigInteger, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_BigIntegerWrapSpec, classOf[WBigInteger], Set(
        "longValueExact", "intValueExact", "shortValueExact", "byteValueExact", "longValue", "intValue", "shortValue", "byteValue", "signum", "negate", "abs", "shiftRight", "shiftLeft", "isProbablePrime", "bitLength", "bitCount", "getLowestSetBit", "flipBit", "clearBit", "setBit", "testBit", "pow", "andNot", "not", "xor", "or", "and", "gcd", "max", "min", "compareTo", "divide", "remainder", "modPow", "modInverse", "mod", "multiply", "subtract", "add", "toByteArray", "toString"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WBigInteger].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WBigInteger] => convertWBigInteger(x) }
      tryConvert(element[WBigInteger], this, x, conv)
    }

    def convertWBigInteger(x: Rep[WBigInteger]): Rep[To] = {
      x.elem match {
        case _: WBigIntegerElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have WBigIntegerElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wBigIntegerElement: Elem[WBigInteger] =
    cachedElem[WBigIntegerElem[WBigInteger]]()

  implicit case object WBigIntegerCompanionElem extends CompanionElem[WBigIntegerCompanionCtor] {
    lazy val tag = weakTypeTag[WBigIntegerCompanionCtor]
    protected def getDefaultRep = RWBigInteger
  }

  abstract class WBigIntegerCompanionCtor extends CompanionDef[WBigIntegerCompanionCtor] with WBigIntegerCompanion {
    def selfType = WBigIntegerCompanionElem
    override def toString = "WBigInteger"
  }
  implicit def proxyWBigIntegerCompanionCtor(p: Rep[WBigIntegerCompanionCtor]): WBigIntegerCompanionCtor =
    proxyOps[WBigIntegerCompanionCtor](p)

  lazy val RWBigInteger: Rep[WBigIntegerCompanionCtor] = new WBigIntegerCompanionCtor {
    private val thisClass = classOf[WBigIntegerCompanion]

    def apply(x$1: Rep[Int], x$2: Rep[WArray[Byte]]): Rep[WBigInteger] =
      newObjEx[WBigInteger](x$1, x$2)

    def apply(x$1: Rep[String]): Rep[WBigInteger] =
      newObjEx[WBigInteger](x$1)

    def valueOf(x$1: Rep[Long]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("valueOf", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]))
    }

    def ONE: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("ONE"),
        List(),
        true, element[WBigInteger]))
    }

    def ZERO: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("ZERO"),
        List(),
        true, element[WBigInteger]))
    }
  }

  object WBigIntegerMethods {
    object longValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "longValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "intValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shortValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "byteValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "longValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "intValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shortValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "byteValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object signum {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "signum" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object negate {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "negate" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object abs {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "abs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftRight {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shiftRight" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftLeft {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shiftLeft" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isProbablePrime {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "isProbablePrime" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitLength {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "bitLength" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitCount {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "bitCount" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getLowestSetBit {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "getLowestSetBit" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flipBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "flipBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object clearBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "clearBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object setBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "setBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object testBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "testBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object pow {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "pow" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object andNot {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "andNot" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object not {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "not" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "xor" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "or" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "and" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object gcd {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "gcd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "max" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "min" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "compareTo" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "divide" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "remainder" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modPow {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "modPow" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modInverse {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "modInverse" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "mod" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "subtract" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toByteArray {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "toByteArray" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // manual fix (method name)
    object toStringMethod {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "toString" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object WBigIntegerCompanionMethods {
    object apply_constructor_1 {
      def unapply(d: Def[_]): Nullable[(Rep[Int], Rep[WArray[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "constructor_1" } =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Int], Rep[WArray[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Int], Rep[WArray[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply_constructor_2 {
      def unapply(d: Def[_]): Nullable[Rep[String]] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "constructor_2" } =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Rep[String]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[String]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueOf {
      def unapply(d: Def[_]): Nullable[Rep[Long]] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "valueOf" =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Rep[Long]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Long]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
// manual fix
//    object ONE {
//      def unapply(d: Def[_]): Nullable[Unit] = d match {
//        case MethodCall(receiver, method, _, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "ONE" =>
//          val res = ()
//          Nullable(res).asInstanceOf[Nullable[Unit]]
//        case _ => Nullable.None
//      }
//      def unapply(exp: Sym): Nullable[Unit] = exp match {
//        case Def(d) => unapply(d)
//        case _ => Nullable.None
//      }
//    }
//
//    object ZERO {
//      def unapply(d: Def[_]): Nullable[Unit] = d match {
//        case MethodCall(receiver, method, _, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "ZERO" =>
//          val res = ()
//          Nullable(res).asInstanceOf[Nullable[Unit]]
//        case _ => Nullable.None
//      }
//      def unapply(exp: Sym): Nullable[Unit] = exp match {
//        case Def(d) => unapply(d)
//        case _ => Nullable.None
//      }
//    }
  }
} // of object WBigInteger
  registerEntityObject("WBigInteger", WBigInteger)

  registerModule(WBigIntegersModule)
}

object WBigIntegersModule extends scalan.ModuleInfo("wrappers.java.math", "WBigIntegers")
}

trait WBigIntegersModule extends wrappers.java.math.impl.WBigIntegersDefs {self: WrappersModule =>}
