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
import Sigma._
import Col._
import CrossChainAtomicSwap._
import InChainAtomicSwap._
import CoinEmission._
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
    object deadline {
      def unapply(d: Def[_]): Option[Rep[CrowdFunding]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrowdFundingElem[_]] && method.getName == "deadline" =>
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

object CrossChainAtomicSwap extends EntityObject("CrossChainAtomicSwap") {
  // entityProxy: single proxy for each type family
  implicit def proxyCrossChainAtomicSwap(p: Rep[CrossChainAtomicSwap]): CrossChainAtomicSwap = {
    proxyOps[CrossChainAtomicSwap](p)(scala.reflect.classTag[CrossChainAtomicSwap])
  }

  // familyElem
  class CrossChainAtomicSwapElem[To <: CrossChainAtomicSwap]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CrossChainAtomicSwap].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CrossChainAtomicSwap] => convertCrossChainAtomicSwap(x) }
      tryConvert(element[CrossChainAtomicSwap], this, x, conv)
    }

    def convertCrossChainAtomicSwap(x: Rep[CrossChainAtomicSwap]): Rep[To] = {
      x.elem match {
        case _: CrossChainAtomicSwapElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have CrossChainAtomicSwapElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def crossChainAtomicSwapElement: Elem[CrossChainAtomicSwap] =
    cachedElem[CrossChainAtomicSwapElem[CrossChainAtomicSwap]]()

  implicit case object CrossChainAtomicSwapCompanionElem extends CompanionElem[CrossChainAtomicSwapCompanionCtor] {
    lazy val tag = weakTypeTag[CrossChainAtomicSwapCompanionCtor]
    protected def getDefaultRep = RCrossChainAtomicSwap
  }

  abstract class CrossChainAtomicSwapCompanionCtor extends CompanionDef[CrossChainAtomicSwapCompanionCtor] with CrossChainAtomicSwapCompanion {
    def selfType = CrossChainAtomicSwapCompanionElem
    override def toString = "CrossChainAtomicSwap"
  }
  implicit def proxyCrossChainAtomicSwapCompanionCtor(p: Rep[CrossChainAtomicSwapCompanionCtor]): CrossChainAtomicSwapCompanionCtor =
    proxyOps[CrossChainAtomicSwapCompanionCtor](p)

  lazy val RCrossChainAtomicSwap: Rep[CrossChainAtomicSwapCompanionCtor] = new CrossChainAtomicSwapCompanionCtor {
  }

  object CrossChainAtomicSwapMethods {
    object deadlineBob {
      def unapply(d: Def[_]): Option[Rep[CrossChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "deadlineBob" =>
          Some(receiver).asInstanceOf[Option[Rep[CrossChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrossChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object deadlineAlice {
      def unapply(d: Def[_]): Option[Rep[CrossChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "deadlineAlice" =>
          Some(receiver).asInstanceOf[Option[Rep[CrossChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrossChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pkA {
      def unapply(d: Def[_]): Option[Rep[CrossChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "pkA" =>
          Some(receiver).asInstanceOf[Option[Rep[CrossChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrossChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pkB {
      def unapply(d: Def[_]): Option[Rep[CrossChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "pkB" =>
          Some(receiver).asInstanceOf[Option[Rep[CrossChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrossChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hx {
      def unapply(d: Def[_]): Option[Rep[CrossChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "hx" =>
          Some(receiver).asInstanceOf[Option[Rep[CrossChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CrossChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object templateForBobChain {
      def unapply(d: Def[_]): Option[(Rep[CrossChainAtomicSwap], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "templateForBobChain" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[CrossChainAtomicSwap], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[CrossChainAtomicSwap], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object templateForAliceChain {
      def unapply(d: Def[_]): Option[(Rep[CrossChainAtomicSwap], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[CrossChainAtomicSwapElem[_]] && method.getName == "templateForAliceChain" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[CrossChainAtomicSwap], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[CrossChainAtomicSwap], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CrossChainAtomicSwapCompanionMethods {
  }
} // of object CrossChainAtomicSwap
  registerEntityObject("CrossChainAtomicSwap", CrossChainAtomicSwap)

object InChainAtomicSwap extends EntityObject("InChainAtomicSwap") {
  // entityProxy: single proxy for each type family
  implicit def proxyInChainAtomicSwap(p: Rep[InChainAtomicSwap]): InChainAtomicSwap = {
    proxyOps[InChainAtomicSwap](p)(scala.reflect.classTag[InChainAtomicSwap])
  }

  // familyElem
  class InChainAtomicSwapElem[To <: InChainAtomicSwap]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[InChainAtomicSwap].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[InChainAtomicSwap] => convertInChainAtomicSwap(x) }
      tryConvert(element[InChainAtomicSwap], this, x, conv)
    }

    def convertInChainAtomicSwap(x: Rep[InChainAtomicSwap]): Rep[To] = {
      x.elem match {
        case _: InChainAtomicSwapElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have InChainAtomicSwapElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def inChainAtomicSwapElement: Elem[InChainAtomicSwap] =
    cachedElem[InChainAtomicSwapElem[InChainAtomicSwap]]()

  implicit case object InChainAtomicSwapCompanionElem extends CompanionElem[InChainAtomicSwapCompanionCtor] {
    lazy val tag = weakTypeTag[InChainAtomicSwapCompanionCtor]
    protected def getDefaultRep = RInChainAtomicSwap
  }

  abstract class InChainAtomicSwapCompanionCtor extends CompanionDef[InChainAtomicSwapCompanionCtor] with InChainAtomicSwapCompanion {
    def selfType = InChainAtomicSwapCompanionElem
    override def toString = "InChainAtomicSwap"
  }
  implicit def proxyInChainAtomicSwapCompanionCtor(p: Rep[InChainAtomicSwapCompanionCtor]): InChainAtomicSwapCompanionCtor =
    proxyOps[InChainAtomicSwapCompanionCtor](p)

  lazy val RInChainAtomicSwap: Rep[InChainAtomicSwapCompanionCtor] = new InChainAtomicSwapCompanionCtor {
  }

  object InChainAtomicSwapMethods {
    object deadline {
      def unapply(d: Def[_]): Option[Rep[InChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[InChainAtomicSwapElem[_]] && method.getName == "deadline" =>
          Some(receiver).asInstanceOf[Option[Rep[InChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[InChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pkA {
      def unapply(d: Def[_]): Option[Rep[InChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[InChainAtomicSwapElem[_]] && method.getName == "pkA" =>
          Some(receiver).asInstanceOf[Option[Rep[InChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[InChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object pkB {
      def unapply(d: Def[_]): Option[Rep[InChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[InChainAtomicSwapElem[_]] && method.getName == "pkB" =>
          Some(receiver).asInstanceOf[Option[Rep[InChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[InChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object token1 {
      def unapply(d: Def[_]): Option[Rep[InChainAtomicSwap]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[InChainAtomicSwapElem[_]] && method.getName == "token1" =>
          Some(receiver).asInstanceOf[Option[Rep[InChainAtomicSwap]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[InChainAtomicSwap]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object templateForAlice {
      def unapply(d: Def[_]): Option[(Rep[InChainAtomicSwap], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[InChainAtomicSwapElem[_]] && method.getName == "templateForAlice" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[InChainAtomicSwap], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[InChainAtomicSwap], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object templateForBob {
      def unapply(d: Def[_]): Option[(Rep[InChainAtomicSwap], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[InChainAtomicSwapElem[_]] && method.getName == "templateForBob" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[InChainAtomicSwap], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[InChainAtomicSwap], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object InChainAtomicSwapCompanionMethods {
  }
} // of object InChainAtomicSwap
  registerEntityObject("InChainAtomicSwap", InChainAtomicSwap)

object CoinEmission extends EntityObject("CoinEmission") {
  // entityProxy: single proxy for each type family
  implicit def proxyCoinEmission(p: Rep[CoinEmission]): CoinEmission = {
    proxyOps[CoinEmission](p)(scala.reflect.classTag[CoinEmission])
  }

  // familyElem
  class CoinEmissionElem[To <: CoinEmission]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CoinEmission].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CoinEmission] => convertCoinEmission(x) }
      tryConvert(element[CoinEmission], this, x, conv)
    }

    def convertCoinEmission(x: Rep[CoinEmission]): Rep[To] = {
      x.elem match {
        case _: CoinEmissionElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have CoinEmissionElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def coinEmissionElement: Elem[CoinEmission] =
    cachedElem[CoinEmissionElem[CoinEmission]]()

  implicit case object CoinEmissionCompanionElem extends CompanionElem[CoinEmissionCompanionCtor] {
    lazy val tag = weakTypeTag[CoinEmissionCompanionCtor]
    protected def getDefaultRep = RCoinEmission
  }

  abstract class CoinEmissionCompanionCtor extends CompanionDef[CoinEmissionCompanionCtor] with CoinEmissionCompanion {
    def selfType = CoinEmissionCompanionElem
    override def toString = "CoinEmission"
  }
  implicit def proxyCoinEmissionCompanionCtor(p: Rep[CoinEmissionCompanionCtor]): CoinEmissionCompanionCtor =
    proxyOps[CoinEmissionCompanionCtor](p)

  lazy val RCoinEmission: Rep[CoinEmissionCompanionCtor] = new CoinEmissionCompanionCtor {
  }

  object CoinEmissionMethods {
    object fixedRatePeriod {
      def unapply(d: Def[_]): Option[Rep[CoinEmission]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CoinEmissionElem[_]] && method.getName == "fixedRatePeriod" =>
          Some(receiver).asInstanceOf[Option[Rep[CoinEmission]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CoinEmission]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object epochLength {
      def unapply(d: Def[_]): Option[Rep[CoinEmission]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CoinEmissionElem[_]] && method.getName == "epochLength" =>
          Some(receiver).asInstanceOf[Option[Rep[CoinEmission]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CoinEmission]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fixedRate {
      def unapply(d: Def[_]): Option[Rep[CoinEmission]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CoinEmissionElem[_]] && method.getName == "fixedRate" =>
          Some(receiver).asInstanceOf[Option[Rep[CoinEmission]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CoinEmission]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object oneEpochReduction {
      def unapply(d: Def[_]): Option[Rep[CoinEmission]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CoinEmissionElem[_]] && method.getName == "oneEpochReduction" =>
          Some(receiver).asInstanceOf[Option[Rep[CoinEmission]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CoinEmission]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object templateForTotalAmountBox {
      def unapply(d: Def[_]): Option[(Rep[CoinEmission], Rep[Context])] = d match {
        case MethodCall(receiver, method, Seq(ctx, _*), _) if receiver.elem.isInstanceOf[CoinEmissionElem[_]] && method.getName == "templateForTotalAmountBox" =>
          Some((receiver, ctx)).asInstanceOf[Option[(Rep[CoinEmission], Rep[Context])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[CoinEmission], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CoinEmissionCompanionMethods {
  }
} // of object CoinEmission
  registerEntityObject("CoinEmission", CoinEmission)

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
