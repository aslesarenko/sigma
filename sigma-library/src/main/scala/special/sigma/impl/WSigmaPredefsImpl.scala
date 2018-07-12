package special.sigma

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSigmaPredefsDefs extends scalan.Scalan with WSigmaPredefs {
  self: WrappersModule =>

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
    protected def getDefaultRep = WSigmaPredef
  }

  abstract class WSigmaPredefCompanionCtor extends CompanionDef[WSigmaPredefCompanionCtor] with WSigmaPredefCompanion {
    def selfType = WSigmaPredefCompanionElem
    override def toString = "WSigmaPredef"
  }
  implicit def proxyWSigmaPredefCompanionCtor(p: Rep[WSigmaPredefCompanionCtor]): WSigmaPredefCompanionCtor =
    proxyOps[WSigmaPredefCompanionCtor](p)

  lazy val WSigmaPredef: Rep[WSigmaPredefCompanionCtor] = new WSigmaPredefCompanionCtor {
    def cost(v: Rep[Any]): Rep[Int] = {
      mkMethodCall(self,
        this.getClass.getMethod("cost", classOf[Sym]),
        List(v),
        true, element[Int]).asRep[Int]
    }
  }

  object WSigmaPredefMethods {
  }

  object WSigmaPredefCompanionMethods {
    object cost {
      def unapply(d: Def[_]): Option[Rep[Any]] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem == WSigmaPredefCompanionElem && method.getName == "cost" =>
          Some(v).asInstanceOf[Option[Rep[Any]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Any]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  registerModule(WSigmaPredefsModule)
}

object WSigmaPredefsModule extends scalan.ModuleInfo("special.sigma", "WSigmaPredefs")
}

trait WSigmaPredefsModule extends special.sigma.impl.WSigmaPredefsDefs {self: WrappersModule =>}
