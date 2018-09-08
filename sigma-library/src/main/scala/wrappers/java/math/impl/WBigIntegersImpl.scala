package wrappers.java.math

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import java.lang.reflect.Method
  import java.math.BigInteger

  import org.bouncycastle.math.ec.ECPoint
  import special.sigma.wrappers.BigIntegerWrapSpec
  import special.wrappers.ArrayWrapSpec

  // Abs -----------------------------------
trait WBigIntegersDefs extends scalan.Scalan with WBigIntegers {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WBigInteger._
import WArray._

object WBigInteger extends EntityObject("WBigInteger") {
  import Liftables._
  case class WBigIntegerConst(constValue: BigInteger) extends WBigInteger with LiftedConst[BigInteger, WBigInteger] {
    val selfType: Elem[WBigInteger] = wBigIntegerElement
    def liftable: Liftable[BigInteger, WBigInteger] = LiftableBigInteger
    def longValueExact: Rep[Long] = delayInvoke
    def intValueExact: Rep[Int] = delayInvoke
    def shortValueExact: Rep[Short] = delayInvoke
    def byteValueExact: Rep[Byte] = delayInvoke
    def longValue: Rep[Long] = delayInvoke
    def intValue: Rep[Int] = delayInvoke
    def shortValue: Rep[Short] = delayInvoke
    def byteValue: Rep[Byte] = delayInvoke
    def signum: Rep[Int] = delayInvoke
    def negate: Rep[WBigInteger] = delayInvoke
    def abs: Rep[WBigInteger] = delayInvoke
    def shiftRight(x$1: Rep[Int]): Rep[WBigInteger] = delayInvoke
    def shiftLeft(x$1: Rep[Int]): Rep[WBigInteger] = delayInvoke
    def isProbablePrime(x$1: Rep[Int]): Rep[Boolean] = delayInvoke
    def bitLength: Rep[Int] = delayInvoke
    def bitCount: Rep[Int] = delayInvoke
    def getLowestSetBit: Rep[Int] = delayInvoke
    def flipBit(x$1: Rep[Int]): Rep[WBigInteger] = delayInvoke
    def clearBit(x$1: Rep[Int]): Rep[WBigInteger] = delayInvoke
    def setBit(x$1: Rep[Int]): Rep[WBigInteger] = delayInvoke
    def testBit(x$1: Rep[Int]): Rep[Boolean] = delayInvoke
    def pow(x$1: Rep[Int]): Rep[WBigInteger] = delayInvoke
    def andNot(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def not: Rep[WBigInteger] = delayInvoke
    def xor(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def or(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def and(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def gcd(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def max(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def min(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def compareTo(x$1: Rep[WBigInteger]): Rep[Int] = delayInvoke
    def divide(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def remainder(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def modPow(x$1: Rep[WBigInteger],
        x$2: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def modInverse(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def mod(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def multiply(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def subtract(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def add(x$1: Rep[WBigInteger]): Rep[WBigInteger] = delayInvoke
    def toByteArray: Rep[WArray[Byte]] = delayInvoke
    def toString(x$1: Rep[Int]): Rep[String] = delayInvoke
  }

  implicit object LiftableBigInteger extends Liftable[BigInteger, WBigInteger] {
    val eW: Elem[WBigInteger] = wBigIntegerElement
    val sourceClassTag = classTag[BigInteger]
    def lift(x: BigInteger): Rep[WBigInteger] = WBigIntegerConst(x)
    def unlift(w: Rep[WBigInteger]): BigInteger = w match {
      case Def(WBigIntegerConst(x: BigInteger)) => x
      case _ => unliftError(w)
    }
  }

  def liftBigInteger(x: BigInteger): Rep[WBigInteger] = liftConst(x)

  // entityProxy: single proxy for each type family
  implicit def proxyWBigInteger(p: Rep[WBigInteger]): WBigInteger = {
    proxyOps[WBigInteger](p)(scala.reflect.classTag[WBigInteger])
  }

  // familyElem
  class WBigIntegerElem[To <: WBigInteger]
    extends EntityElem[To] {
    override def liftable: Liftable[_, To] = LiftableBigInteger.asLiftable[BigInteger, To]

    override protected def collectMethods: Map[Method, MethodDesc] = {
      val wrapCls = classOf[BigIntegerWrapSpec]
      val srcCls = classOf[BigInteger]
      val cls = classOf[WBigInteger]
      val spec = new BigIntegerWrapSpec
      super.collectMethods ++ Seq(
        cls.getMethod("add", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("add", srcCls, classOf[BigInteger])),
        cls.getMethod("multiply", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("multiply", srcCls, classOf[BigInteger])),
      )
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
        case _: WBigIntegerElem[_] => x.asRep[To]
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
    def apply(x$1: Rep[Int], x$2: Rep[WArray[Byte]]): Rep[WBigInteger] =
      newObjEx[WBigInteger](x$1, x$2)

    def apply(x$1: Rep[String]): Rep[WBigInteger] =
      newObjEx[WBigInteger](x$1)

    def valueOf(x$1: Rep[Long]): Rep[WBigInteger] = {
      mkMethodCall(self,
        this.getClass.getMethod("valueOf", classOf[Sym]),
        List(x$1),
        true, element[WBigInteger]).asRep[WBigInteger]
    }

    def ONE: Rep[WBigInteger] = {
      mkMethodCall(self,
        this.getClass.getMethod("ONE"),
        List(),
        true, element[WBigInteger]).asRep[WBigInteger]
    }

    def ZERO: Rep[WBigInteger] = {
      mkMethodCall(self,
        this.getClass.getMethod("ZERO"),
        List(),
        true, element[WBigInteger]).asRep[WBigInteger]
    }
  }

  object WBigIntegerMethods {
    object longValueExact {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "longValueExact" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object intValueExact {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "intValueExact" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shortValueExact {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shortValueExact" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteValueExact {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "byteValueExact" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longValue {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "longValue" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object intValue {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "intValue" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shortValue {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shortValue" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object byteValue {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "byteValue" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object signum {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "signum" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object negate {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "negate" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object abs {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "abs" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shiftRight {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shiftRight" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shiftLeft {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shiftLeft" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isProbablePrime {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "isProbablePrime" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bitLength {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "bitLength" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bitCount {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "bitCount" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getLowestSetBit {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "getLowestSetBit" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flipBit {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "flipBit" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object clearBit {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "clearBit" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object setBit {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "setBit" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object testBit {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "testBit" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pow {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "pow" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object andNot {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "andNot" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object not {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "not" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object xor {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "xor" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "or" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "and" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object gcd {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "gcd" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object max {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "max" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object min {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "min" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "compareTo" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object divide {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "divide" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "remainder" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object modPow {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, x$2, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "modPow" =>
          Some((receiver, x$1, x$2)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object modInverse {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "modInverse" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mod {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "mod" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "multiply" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "subtract" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "add" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toByteArray {
      def unapply(d: Def[_]): Option[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "toByteArray" =>
          Some(receiver).asInstanceOf[Option[Rep[WBigInteger]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toStringMethod {
      def unapply(d: Def[_]): Option[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "toString" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WBigInteger], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WBigIntegerCompanionMethods {
    object apply_constructor_1 {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[WArray[Byte]])] = d match {
        case MethodCall(receiver, method, Seq(x$1, x$2, _*), _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "constructor_1" } =>
          Some((x$1, x$2)).asInstanceOf[Option[(Rep[Int], Rep[WArray[Byte]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Int], Rep[WArray[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_constructor_2 {
      def unapply(d: Def[_]): Option[Rep[String]] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "constructor_2" } =>
          Some(x$1).asInstanceOf[Option[Rep[String]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[String]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object valueOf {
      def unapply(d: Def[_]): Option[Rep[Long]] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "valueOf" =>
          Some(x$1).asInstanceOf[Option[Rep[Long]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Long]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object ONE {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "ONE" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object ZERO {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "ZERO" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
} // of object WBigInteger
  registerEntityObject("WBigInteger", WBigInteger)

  registerModule(WBigIntegersModule)
}

object WBigIntegersModule extends scalan.ModuleInfo("wrappers.java.math", "WBigIntegers")
}

trait WBigIntegersModule extends wrappers.java.math.impl.WBigIntegersDefs {self: WrappersModule =>}
