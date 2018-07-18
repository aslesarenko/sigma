package scalan

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint
import special.sigma._
import special.sigma.wrappers.WrappersSpecModule

import scala.collection.mutable.ArrayBuffer

trait SigmaLibrary extends Library
  with special.sigma.wrappers.WrappersModule
  with WrappersSpecModule
  with SigmaDslModule
  with SigmaExamplesModule
  with SigmaDslOverArraysModule
  with TestContractsModule {
  import Context._; import WArray._; import Col._; import ReplCol._; import ColBuilder._;
  import Sigma._; import SigmaContract._; import Box._
  import WECPoint._; import ColOverArrayBuilder._; import ConcreteCostedBuilder._
  import Costed._; import CostedPrim._; import CostedPair._;
  import CostedArray._; import CostedNestedArray._; import CostedPairArray._
  import CostedCol._; import CostedNestedCol._; import CostedPairCol._
  import ProveDlogEvidence._

  implicit lazy val EcPointElement: Elem[ECPoint] = new BaseElem(CustomNamedCurves.getByName("curve25519").getG)

  val WA = WArrayMethods
  val CM = ColMethods
  val CBM = ColBuilderMethods
  val SM = SigmaMethods
  val SCM = SigmaContractMethods

  object AnyOf {
    def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[ColBuilder], Seq[Rep[A]]) forSome {type A}] = d match {
      case SCM.anyOf(c, CBM.apply_apply_items(b, items)) =>
        Some((c, b, items))
      case _ => None
    }
  }
  object AllOf {
    def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[ColBuilder], Seq[Rep[A]]) forSome {type A}] = d match {
      case SCM.allOf(c, CBM.apply_apply_items(b, items)) =>
        Some((c, b, items))
      case _ => None
    }
  }
  object AnyZk {
    def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[ColBuilder], Seq[Rep[Sigma]])] = d match {
      case SCM.anyZK(c, CBM.apply_apply_items(b, items)) =>
        Some((c, b, items.asInstanceOf[Seq[Rep[Sigma]]]))
      case _ => None
    }
  }
  object AllZk {
    def unapply(d: Def[_]): Option[(Rep[SigmaContract], Rep[ColBuilder], Seq[Rep[Sigma]])] = d match {
      case SCM.allZK(c, CBM.apply_apply_items(b, items)) =>
        Some((c, b, items.asInstanceOf[Seq[Rep[Sigma]]]))
      case _ => None
    }
  }
  object HasSigmas {
    def unapply(items: Seq[Sym]): Option[(Seq[Rep[Boolean]], Seq[Rep[Sigma]])] = {
      val bs = ArrayBuffer.empty[BoolRep]
      val ss = ArrayBuffer.empty[Rep[Sigma]]
      for (i <- items) {
        i match {
          case SM.isValid(s) => ss += s
          case b => bs += b.asRep[Boolean]
        }
      }
      assert(items.length == bs.length + ss.length)
      if (ss.isEmpty) None
      else Some((bs,ss))
    }
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case AllOf(c, b, HasSigmas(bools, sigmas)) =>
      val zkAll = c.allZK(b.apply(sigmas:_*))
      if (bools.isEmpty)
        zkAll.isValid
      else
        (zkAll && c.allOf(b.apply(bools:_*))).isValid
    case AnyOf(c, b, HasSigmas(bs, ss)) =>
      val zkAny = c.anyZK(b.apply(ss:_*))
      if (bs.isEmpty)
        zkAny.isValid
      else
        (zkAny || c.anyOf(b.apply(bs:_*))).isValid
    case AllOf(_,_,items) if items.length == 1 => items(0)
    case AnyOf(_,_,items) if items.length == 1 => items(0)
    case AllZk(_,_,items) if items.length == 1 => items(0)
    case AnyZk(_,_,items) if items.length == 1 => items(0)

    //    case SM.and_bool_&&(SCM.allZK(sigmas), b) =>
    case _ =>
      if (currentPass.config.constantPropagation) {
        // additional constant propagation rules (see other similar cases)
        d match {
          case AnyOf(_,_,items) if (items.forall(_.isConst)) =>
            val bs = items.map { case Def(Const(b: Boolean)) => b }
            toRep(bs.exists(_ == true))
          case AllOf(_,_,items) if (items.forall(_.isConst)) =>
            val bs = items.map { case Def(Const(b: Boolean)) => b }
            toRep(bs.forall(_ == true))
          case _ =>
            super.rewriteDef(d)
        }
      }
      else
        super.rewriteDef(d)
  }

  override def toRep[A](x: A)(implicit eA: Elem[A]):Rep[A] = eA match {
    case EcPointElement => Const(x)
    case _ => super.toRep(x)
  }

  case class WEcPointNew(p: Rep[ECPoint]) extends Def[WECPoint] {
    def selfType: Elem[WECPoint] = wECPointElement
  }

}
