package wrappers.org.bouncycastle.math.ec

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WECPointsDefs extends scalan.Scalan with WECPoints {
  self: WrappersModule =>

  // entityProxy: single proxy for each type family
  implicit def proxyWECPoint(p: Rep[WECPoint]): WECPoint = {
    proxyOps[WECPoint](p)(scala.reflect.classTag[WECPoint])
  }

  // familyElem
  class WECPointElem[To <: WECPoint]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WECPoint].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WECPoint] => convertWECPoint(x) }
      tryConvert(element[WECPoint], this, x, conv)
    }

    def convertWECPoint(x: Rep[WECPoint]): Rep[To] = {
      x.elem match {
        case _: WECPointElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have WECPointElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wECPointElement: Elem[WECPoint] =
    cachedElem[WECPointElem[WECPoint]]()

  implicit case object WECPointCompanionElem extends CompanionElem[WECPointCompanionCtor] {
    lazy val tag = weakTypeTag[WECPointCompanionCtor]
    protected def getDefaultRep = WECPoint
  }

  abstract class WECPointCompanionCtor extends CompanionDef[WECPointCompanionCtor] with WECPointCompanion {
    def selfType = WECPointCompanionElem
    override def toString = "WECPoint"
  }
  implicit def proxyWECPointCompanionCtor(p: Rep[WECPointCompanionCtor]): WECPointCompanionCtor =
    proxyOps[WECPointCompanionCtor](p)

  lazy val WECPoint: Rep[WECPointCompanionCtor] = new WECPointCompanionCtor {
  }

  object WECPointMethods {
    object getEncoded {
      def unapply(d: Def[_]): Option[(Rep[WECPoint], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(x$1, _*), _) if receiver.elem.isInstanceOf[WECPointElem[_]] && method.getName == "getEncoded" =>
          Some((receiver, x$1)).asInstanceOf[Option[(Rep[WECPoint], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WECPoint], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WECPointCompanionMethods {
  }

  registerModule(WECPointsModule)
}

object WECPointsModule extends scalan.ModuleInfo("org.bouncycastle.math.ec", "WECPoints")
}

trait WECPointsModule extends wrappers.org.bouncycastle.math.ec.impl.WECPointsDefs {self: WrappersModule =>}
