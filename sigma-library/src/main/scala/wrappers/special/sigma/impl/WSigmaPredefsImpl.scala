package wrappers.special.sigma

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSigmaPredefsDefs extends scalan.Scalan with WSigmaPredefs {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WSigmaPredef._

object WSigmaPredef extends EntityObject("WSigmaPredef") {
  // entityProxy: single proxy for each type family
  implicit def proxyWSigmaPredef(p: Rep[WSigmaPredef]): WSigmaPredef = {
    proxyOps[WSigmaPredef](p)(scala.reflect.classTag[WSigmaPredef])
  }

  // familyElem
  class WSigmaPredefElem[To <: WSigmaPredef]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WSigmaPredef].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WSigmaPredef] => convertWSigmaPredef(x) }
      tryConvert(element[WSigmaPredef], this, x, conv)
    }

    def convertWSigmaPredef(x: Rep[WSigmaPredef]): Rep[To] = {
      x.elem match {
        case _: WSigmaPredefElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have WSigmaPredefElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wSigmaPredefElement: Elem[WSigmaPredef] =
    cachedElem[WSigmaPredefElem[WSigmaPredef]]()

  implicit case object WSigmaPredefCompanionElem extends CompanionElem[WSigmaPredefCompanionCtor] {
    lazy val tag = weakTypeTag[WSigmaPredefCompanionCtor]
    protected def getDefaultRep = RWSigmaPredef
  }

  abstract class WSigmaPredefCompanionCtor extends CompanionDef[WSigmaPredefCompanionCtor] with WSigmaPredefCompanion {
    def selfType = WSigmaPredefCompanionElem
    override def toString = "WSigmaPredef"
  }
  implicit def proxyWSigmaPredefCompanionCtor(p: Rep[WSigmaPredefCompanionCtor]): WSigmaPredefCompanionCtor =
    proxyOps[WSigmaPredefCompanionCtor](p)

  lazy val RWSigmaPredef: Rep[WSigmaPredefCompanionCtor] = new WSigmaPredefCompanionCtor {
    def dataSize[T](v: Rep[T]): Rep[Long] = {
      implicit val eT = v.elem
      mkMethodCall(self,
        this.getClass.getMethod("dataSize", classOf[Sym]),
        List(v),
        true, element[Long]).asRep[Long]
    }
  }

  object WSigmaPredefMethods {
  }

  object WSigmaPredefCompanionMethods {
    object dataSize {
      def unapply(d: Def[_]): Option[Rep[T] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem == WSigmaPredefCompanionElem && method.getName == "dataSize" =>
          Some(v).asInstanceOf[Option[Rep[T] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[T] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
} // of object WSigmaPredef
  registerEntityObject("WSigmaPredef", WSigmaPredef)

  registerModule(WSigmaPredefsModule)
}

object WSigmaPredefsModule extends scalan.ModuleInfo("wrappers.special.sigma", "WSigmaPredefs")
}

trait WSigmaPredefsModule extends wrappers.special.sigma.impl.WSigmaPredefsDefs {self: WrappersModule =>}
