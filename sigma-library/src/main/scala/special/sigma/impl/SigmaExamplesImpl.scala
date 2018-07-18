package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaExamplesDefs extends scalan.Scalan with SigmaExamples {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import ProveDlog._
import CrowdFunding._
import SigmaContract._
import DemurrageCurrency._

object CrowdFunding extends EntityObject("CrowdFunding") {
  // entityProxy: single proxy for each type family
  implicit def proxyCrowdFunding(p: Rep[CrowdFunding]): CrowdFunding = {
    proxyOps[CrowdFunding](p)(scala.reflect.classTag[CrowdFunding])
  }

  // familyElem
  class CrowdFundingElem[To <: CrowdFunding]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CrowdFunding].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CrowdFunding] => convertCrowdFunding(x) }
      tryConvert(element[CrowdFunding], this, x, conv)
    }

    def convertCrowdFunding(x: Rep[CrowdFunding]): Rep[To] = {
      x.elem match {
        case _: CrowdFundingElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have CrowdFundingElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def crowdFundingElement: Elem[CrowdFunding] =
    cachedElem[CrowdFundingElem[CrowdFunding]]()

  implicit case object CrowdFundingCompanionElem extends CompanionElem[CrowdFundingCompanionCtor] {
    lazy val tag = weakTypeTag[CrowdFundingCompanionCtor]
    protected def getDefaultRep = RCrowdFunding
  }

  abstract class CrowdFundingCompanionCtor extends CompanionDef[CrowdFundingCompanionCtor] with CrowdFundingCompanion {
    def selfType = CrowdFundingCompanionElem
    override def toString = "CrowdFunding"
  }
  implicit def proxyCrowdFundingCompanionCtor(p: Rep[CrowdFundingCompanionCtor]): CrowdFundingCompanionCtor =
    proxyOps[CrowdFundingCompanionCtor](p)

  lazy val RCrowdFunding: Rep[CrowdFundingCompanionCtor] = new CrowdFundingCompanionCtor {
  }

  object CrowdFundingMethods {
    object timeout {
      def unapply(d: Def[_]): Option[Rep[CrowdFunding]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrowdFundingElem[_]] && method.getName == "timeout" =>
          Some(receiver).asInstanceOf[Option[Rep[CrowdFunding]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrowdFunding]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object minToRaise {
      def unapply(d: Def[_]): Option[Rep[CrowdFunding]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrowdFundingElem[_]] && method.getName == "minToRaise" =>
          Some(receiver).asInstanceOf[Option[Rep[CrowdFunding]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrowdFunding]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object backerPubKey {
      def unapply(d: Def[_]): Option[Rep[CrowdFunding]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrowdFundingElem[_]] && method.getName == "backerPubKey" =>
          Some(receiver).asInstanceOf[Option[Rep[CrowdFunding]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrowdFunding]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object projectPubKey {
      def unapply(d: Def[_]): Option[Rep[CrowdFunding]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrowdFundingElem[_]] && method.getName == "projectPubKey" =>
          Some(receiver).asInstanceOf[Option[Rep[CrowdFunding]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrowdFunding]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object canOpen {
      def unapply(d: Def[_]): Option[(Rep[CrowdFunding], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[CrowdFundingElem[_]] && method.getName == "canOpen" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[CrowdFunding], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[CrowdFunding], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CrowdFundingCompanionMethods {
  }
} // of object CrowdFunding
  registerEntityObject("CrowdFunding", CrowdFunding)

object DemurrageCurrency extends EntityObject("DemurrageCurrency") {
  // entityProxy: single proxy for each type family
  implicit def proxyDemurrageCurrency(p: Rep[DemurrageCurrency]): DemurrageCurrency = {
    proxyOps[DemurrageCurrency](p)(scala.reflect.classTag[DemurrageCurrency])
  }

  // familyElem
  class DemurrageCurrencyElem[To <: DemurrageCurrency]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DemurrageCurrency].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DemurrageCurrency] => convertDemurrageCurrency(x) }
      tryConvert(element[DemurrageCurrency], this, x, conv)
    }

    def convertDemurrageCurrency(x: Rep[DemurrageCurrency]): Rep[To] = {
      x.elem match {
        case _: DemurrageCurrencyElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DemurrageCurrencyElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def demurrageCurrencyElement: Elem[DemurrageCurrency] =
    cachedElem[DemurrageCurrencyElem[DemurrageCurrency]]()

  implicit case object DemurrageCurrencyCompanionElem extends CompanionElem[DemurrageCurrencyCompanionCtor] {
    lazy val tag = weakTypeTag[DemurrageCurrencyCompanionCtor]
    protected def getDefaultRep = RDemurrageCurrency
  }

  abstract class DemurrageCurrencyCompanionCtor extends CompanionDef[DemurrageCurrencyCompanionCtor] with DemurrageCurrencyCompanion {
    def selfType = DemurrageCurrencyCompanionElem
    override def toString = "DemurrageCurrency"
  }
  implicit def proxyDemurrageCurrencyCompanionCtor(p: Rep[DemurrageCurrencyCompanionCtor]): DemurrageCurrencyCompanionCtor =
    proxyOps[DemurrageCurrencyCompanionCtor](p)

  lazy val RDemurrageCurrency: Rep[DemurrageCurrencyCompanionCtor] = new DemurrageCurrencyCompanionCtor {
  }

  object DemurrageCurrencyMethods {
    object demurragePeriod {
      def unapply(d: Def[_]): Option[Rep[DemurrageCurrency]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DemurrageCurrencyElem[_]] && method.getName == "demurragePeriod" =>
          Some(receiver).asInstanceOf[Option[Rep[DemurrageCurrency]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DemurrageCurrency]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object demurrageCost {
      def unapply(d: Def[_]): Option[Rep[DemurrageCurrency]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DemurrageCurrencyElem[_]] && method.getName == "demurrageCost" =>
          Some(receiver).asInstanceOf[Option[Rep[DemurrageCurrency]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DemurrageCurrency]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object regScript {
      def unapply(d: Def[_]): Option[Rep[DemurrageCurrency]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DemurrageCurrencyElem[_]] && method.getName == "regScript" =>
          Some(receiver).asInstanceOf[Option[Rep[DemurrageCurrency]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DemurrageCurrency]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object canOpen {
      def unapply(d: Def[_]): Option[(Rep[DemurrageCurrency], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[DemurrageCurrencyElem[_]] && method.getName == "canOpen" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[DemurrageCurrency], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DemurrageCurrency], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DemurrageCurrencyCompanionMethods {
  }
} // of object DemurrageCurrency
  registerEntityObject("DemurrageCurrency", DemurrageCurrency)

  registerModule(SigmaExamplesModule)
}

object SigmaExamplesModule extends scalan.ModuleInfo("special.sigma", "SigmaExamples")
}

trait SigmaExamplesModule extends special.sigma.impl.SigmaExamplesDefs {self: SigmaLibrary =>}
