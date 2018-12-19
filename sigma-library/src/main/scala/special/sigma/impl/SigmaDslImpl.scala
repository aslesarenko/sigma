package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import scalan.OverloadHack.Overloaded1 // manual fix

  // Abs -----------------------------------
trait SigmaDslDefs extends scalan.Scalan with SigmaDsl {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AnyValue._
import AvlTree._
import Box._
import Col._
import ColBuilder._
import Context._
import CostModel._
import CostedBuilder._
import CostedCol._
import CostedOption._
import DslBuilder._
import DslObject._
import MonoidBuilder._
import SigmaContract._
import SigmaDslBuilder._
import SigmaProp._
import WBigInteger._
import WECPoint._
import WOption._

object CostModel extends EntityObject("CostModel") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SCostModel = special.sigma.CostModel
  case class CostModelConst(
        constValue: SCostModel
      ) extends CostModel with LiftedConst[SCostModel, CostModel] {
    val liftable: Liftable[SCostModel, CostModel] = LiftableCostModel
    val selfType: Elem[CostModel] = liftable.eW
    private val thisClass = classOf[CostModel]

    def AccessBox: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("AccessBox"),
        List(),
        true, false, element[Int]))
    }

    def AccessAvlTree: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("AccessAvlTree"),
        List(),
        true, false, element[Int]))
    }

    def GetVar: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("GetVar"),
        List(),
        true, false, element[Int]))
    }

    def DeserializeVar: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("DeserializeVar"),
        List(),
        true, false, element[Int]))
    }

    def GetRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("GetRegister"),
        List(),
        true, false, element[Int]))
    }

    def DeserializeRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("DeserializeRegister"),
        List(),
        true, false, element[Int]))
    }

    def SelectField: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("SelectField"),
        List(),
        true, false, element[Int]))
    }

    def CollectionConst: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("CollectionConst"),
        List(),
        true, false, element[Int]))
    }

    def AccessKiloByteOfData: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("AccessKiloByteOfData"),
        List(),
        true, false, element[Int]))
    }

    def dataSize[T](x: Rep[T])(implicit cT: Elem[T]): Rep[Long] = {
      implicit val eT = x.elem
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize", classOf[Sym], classOf[Elem[_]]),
        List(x, cT),
        true, false, element[Long]))
    }
  }

  implicit object LiftableCostModel
    extends Liftable[SCostModel, CostModel] {
    lazy val eW: Elem[CostModel] = costModelElement
    lazy val sourceClassTag: ClassTag[SCostModel] = {
      classTag[SCostModel]
    }
    def lift(x: SCostModel): Rep[CostModel] = CostModelConst(x)
    def unlift(w: Rep[CostModel]): SCostModel = w match {
      case Def(CostModelConst(x: SCostModel))
            => x.asInstanceOf[SCostModel]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for CostModel trait
  case class CostModelAdapter(source: Rep[CostModel])
      extends CostModel with Def[CostModel] {
    val selfType: Elem[CostModel] = element[CostModel]
    private val thisClass = classOf[CostModel]

    def AccessBox: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("AccessBox"),
        List(),
        true, true, element[Int]))
    }

    def AccessAvlTree: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("AccessAvlTree"),
        List(),
        true, true, element[Int]))
    }

    def GetVar: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("GetVar"),
        List(),
        true, true, element[Int]))
    }

    def DeserializeVar: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("DeserializeVar"),
        List(),
        true, true, element[Int]))
    }

    def GetRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("GetRegister"),
        List(),
        true, true, element[Int]))
    }

    def DeserializeRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("DeserializeRegister"),
        List(),
        true, true, element[Int]))
    }

    def SelectField: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("SelectField"),
        List(),
        true, true, element[Int]))
    }

    def CollectionConst: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("CollectionConst"),
        List(),
        true, true, element[Int]))
    }

    def AccessKiloByteOfData: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("AccessKiloByteOfData"),
        List(),
        true, true, element[Int]))
    }

    def dataSize[T](x: Rep[T])(implicit cT: Elem[T]): Rep[Long] = {
      implicit val eT = x.elem
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize", classOf[Sym], classOf[Elem[_]]),
        List(x, cT),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostModel(p: Rep[CostModel]): CostModel = {
    if (p.rhs.isInstanceOf[CostModel@unchecked]) p.rhs.asInstanceOf[CostModel]
    else
      CostModelAdapter(p)
  }

  // familyElem
  class CostModelElem[To <: CostModel]
    extends EntityElem[To] {
    override val liftable = LiftableCostModel.asLiftable[SCostModel, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[CostModel], classOf[SCostModel], Set(
        "AccessBox", "AccessAvlTree", "GetVar", "DeserializeVar", "GetRegister", "DeserializeRegister", "SelectField", "CollectionConst", "AccessKiloByteOfData", "dataSize", "PubKeySize"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CostModel].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostModel] => convertCostModel(x) }
      tryConvert(element[CostModel], this, x, conv)
    }

    def convertCostModel(x: Rep[CostModel]): Rep[To] = {
      x.elem match {
        case _: CostModelElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostModelElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costModelElement: Elem[CostModel] =
    cachedElem[CostModelElem[CostModel]]()

  implicit case object CostModelCompanionElem extends CompanionElem[CostModelCompanionCtor] {
    lazy val tag = weakTypeTag[CostModelCompanionCtor]
    protected def getDefaultRep = RCostModel
  }

  abstract class CostModelCompanionCtor extends CompanionDef[CostModelCompanionCtor] with CostModelCompanion {
    def selfType = CostModelCompanionElem
    override def toString = "CostModel"
  }
  implicit def proxyCostModelCompanionCtor(p: Rep[CostModelCompanionCtor]): CostModelCompanionCtor =
    proxyOps[CostModelCompanionCtor](p)

  lazy val RCostModel: Rep[CostModelCompanionCtor] = new CostModelCompanionCtor {
    private val thisClass = classOf[CostModelCompanion]
  }

  object CostModelMethods {
    object AccessBox {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "AccessBox" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object AccessAvlTree {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "AccessAvlTree" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object GetVar {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "GetVar" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object DeserializeVar {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "DeserializeVar" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object GetRegister {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "GetRegister" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object DeserializeRegister {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "DeserializeRegister" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SelectField {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "SelectField" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object CollectionConst {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "CollectionConst" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object AccessKiloByteOfData {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "AccessKiloByteOfData" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[(Rep[CostModel], Rep[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "dataSize" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostModel], Rep[T], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostModel], Rep[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKeySize {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "PubKeySize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostModelCompanionMethods {
  }
} // of object CostModel
  registerEntityObject("CostModel", CostModel)

object DslBuilder extends EntityObject("DslBuilder") {
  // entityAdapter for DslBuilder trait
  case class DslBuilderAdapter(source: Rep[DslBuilder])
      extends DslBuilder with Def[DslBuilder] {
    val selfType: Elem[DslBuilder] = element[DslBuilder]
  }

  // entityProxy: single proxy for each type family
  implicit def proxyDslBuilder(p: Rep[DslBuilder]): DslBuilder = {
    if (p.rhs.isInstanceOf[DslBuilder@unchecked]) p.rhs.asInstanceOf[DslBuilder]
    else
      DslBuilderAdapter(p)
  }

  // familyElem
  class DslBuilderElem[To <: DslBuilder]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DslBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DslBuilder] => convertDslBuilder(x) }
      tryConvert(element[DslBuilder], this, x, conv)
    }

    def convertDslBuilder(x: Rep[DslBuilder]): Rep[To] = {
      x.elem match {
        case _: DslBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have DslBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def dslBuilderElement: Elem[DslBuilder] =
    cachedElem[DslBuilderElem[DslBuilder]]()

  implicit case object DslBuilderCompanionElem extends CompanionElem[DslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[DslBuilderCompanionCtor]
    protected def getDefaultRep = RDslBuilder
  }

  abstract class DslBuilderCompanionCtor extends CompanionDef[DslBuilderCompanionCtor] with DslBuilderCompanion {
    def selfType = DslBuilderCompanionElem
    override def toString = "DslBuilder"
  }
  implicit def proxyDslBuilderCompanionCtor(p: Rep[DslBuilderCompanionCtor]): DslBuilderCompanionCtor =
    proxyOps[DslBuilderCompanionCtor](p)

  lazy val RDslBuilder: Rep[DslBuilderCompanionCtor] = new DslBuilderCompanionCtor {
    private val thisClass = classOf[DslBuilderCompanion]
  }

  object DslBuilderMethods {
  }

  object DslBuilderCompanionMethods {
  }
} // of object DslBuilder
  registerEntityObject("DslBuilder", DslBuilder)

object DslObject extends EntityObject("DslObject") {
  // entityAdapter for DslObject trait
  case class DslObjectAdapter(source: Rep[DslObject])
      extends DslObject with Def[DslObject] {
    val selfType: Elem[DslObject] = element[DslObject]
    private val thisClass = classOf[DslObject]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyDslObject(p: Rep[DslObject]): DslObject = {
    if (p.rhs.isInstanceOf[DslObject@unchecked]) p.rhs.asInstanceOf[DslObject]
    else
      DslObjectAdapter(p)
  }

  // familyElem
  class DslObjectElem[To <: DslObject]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DslObject].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DslObject] => convertDslObject(x) }
      tryConvert(element[DslObject], this, x, conv)
    }

    def convertDslObject(x: Rep[DslObject]): Rep[To] = {
      x.elem match {
        case _: DslObjectElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have DslObjectElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def dslObjectElement: Elem[DslObject] =
    cachedElem[DslObjectElem[DslObject]]()

  implicit case object DslObjectCompanionElem extends CompanionElem[DslObjectCompanionCtor] {
    lazy val tag = weakTypeTag[DslObjectCompanionCtor]
    protected def getDefaultRep = RDslObject
  }

  abstract class DslObjectCompanionCtor extends CompanionDef[DslObjectCompanionCtor] with DslObjectCompanion {
    def selfType = DslObjectCompanionElem
    override def toString = "DslObject"
  }
  implicit def proxyDslObjectCompanionCtor(p: Rep[DslObjectCompanionCtor]): DslObjectCompanionCtor =
    proxyOps[DslObjectCompanionCtor](p)

  lazy val RDslObject: Rep[DslObjectCompanionCtor] = new DslObjectCompanionCtor {
    private val thisClass = classOf[DslObjectCompanion]
  }

  object DslObjectMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[DslObject]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DslObjectElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[DslObject]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[DslObject]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object DslObjectCompanionMethods {
  }
} // of object DslObject
  registerEntityObject("DslObject", DslObject)

object SigmaProp extends EntityObject("SigmaProp") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaProp = special.sigma.SigmaProp
  case class SigmaPropConst(
        constValue: SSigmaProp
      ) extends SigmaProp with LiftedConst[SSigmaProp, SigmaProp] {
    val liftable: Liftable[SSigmaProp, SigmaProp] = LiftableSigmaProp
    val selfType: Elem[SigmaProp] = liftable.eW
    private val thisClass = classOf[SigmaProp]

    // manual fix (builder is inherited)
    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        thisClass.getMethod("builder"),
        List(),
        true, isAdapterCall = false, element[SigmaDslBuilder]))
    }

    def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isValid"),
        List(),
        true, false, element[Boolean]))
    }

    def propBytes: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("propBytes"),
        List(),
        true, false, element[Col[Byte]]))
    }

    // manual fix &&
    def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    // manual fix &&
    def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyAnd", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyOr", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }
  }

  implicit object LiftableSigmaProp
    extends Liftable[SSigmaProp, SigmaProp] {
    lazy val eW: Elem[SigmaProp] = sigmaPropElement
    lazy val sourceClassTag: ClassTag[SSigmaProp] = {
      classTag[SSigmaProp]
    }
    def lift(x: SSigmaProp): Rep[SigmaProp] = SigmaPropConst(x)
    def unlift(w: Rep[SigmaProp]): SSigmaProp = w match {
      case Def(SigmaPropConst(x: SSigmaProp))
            => x.asInstanceOf[SSigmaProp]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for SigmaProp trait
  case class SigmaPropAdapter(source: Rep[SigmaProp])
      extends SigmaProp with Def[SigmaProp] {
    val selfType: Elem[SigmaProp] = element[SigmaProp]
    private val thisClass = classOf[SigmaProp]

    def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isValid"),
        List(),
        true, true, element[Boolean]))
    }

    def propBytes: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("propBytes"),
        List(),
        true, true, element[Col[Byte]]))
    }

    // manual fix &&
    def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    // manual fix &&
    def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, true, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, true, element[SigmaProp]))
    }

    def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("lazyAnd", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("lazyOr", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaProp(p: Rep[SigmaProp]): SigmaProp = {
    if (p.rhs.isInstanceOf[SigmaProp@unchecked]) p.rhs.asInstanceOf[SigmaProp]
    else
      SigmaPropAdapter(p)
  }

  // familyElem
  class SigmaPropElem[To <: SigmaProp]
    extends DslObjectElem[To] {
    override val liftable = LiftableSigmaProp.asLiftable[SSigmaProp, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaProp], classOf[SSigmaProp], Set(
        "isValid", "propBytes", "$amp$amp", "$amp$amp", "$bar$bar", "$bar$bar", "lazyAnd", "lazyOr"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslObjectElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaProp].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaProp] => convertSigmaProp(x) }
      tryConvert(element[SigmaProp], this, x, conv)
    }

    def convertSigmaProp(x: Rep[SigmaProp]): Rep[To] = {
      x.elem match {
        case _: SigmaPropElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaPropElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaPropElement: Elem[SigmaProp] =
    cachedElem[SigmaPropElem[SigmaProp]]()

  implicit case object SigmaPropCompanionElem extends CompanionElem[SigmaPropCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaPropCompanionCtor]
    protected def getDefaultRep = RSigmaProp
  }

  abstract class SigmaPropCompanionCtor extends CompanionDef[SigmaPropCompanionCtor] with SigmaPropCompanion {
    def selfType = SigmaPropCompanionElem
    override def toString = "SigmaProp"
  }
  implicit def proxySigmaPropCompanionCtor(p: Rep[SigmaPropCompanionCtor]): SigmaPropCompanionCtor =
    proxyOps[SigmaPropCompanionCtor](p)

  lazy val RSigmaProp: Rep[SigmaPropCompanionCtor] = new SigmaPropCompanionCtor {
    private val thisClass = classOf[SigmaPropCompanion]
  }

  object SigmaPropMethods {
    object isValid {
      def unapply(d: Def[_]): Nullable[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "isValid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "propBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "lazyAnd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "lazyOr" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaPropCompanionMethods {
  }
} // of object SigmaProp
  registerEntityObject("SigmaProp", SigmaProp)

object AnyValue extends EntityObject("AnyValue") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SAnyValue = special.sigma.AnyValue
  case class AnyValueConst(
        constValue: SAnyValue
      ) extends AnyValue with LiftedConst[SAnyValue, AnyValue] {
    val liftable: Liftable[SAnyValue, AnyValue] = LiftableAnyValue
    val selfType: Elem[AnyValue] = liftable.eW
    private val thisClass = classOf[AnyValue]

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }

  implicit object LiftableAnyValue
    extends Liftable[SAnyValue, AnyValue] {
    lazy val eW: Elem[AnyValue] = anyValueElement
    lazy val sourceClassTag: ClassTag[SAnyValue] = {
      classTag[SAnyValue]
    }
    def lift(x: SAnyValue): Rep[AnyValue] = AnyValueConst(x)
    def unlift(w: Rep[AnyValue]): SAnyValue = w match {
      case Def(AnyValueConst(x: SAnyValue))
            => x.asInstanceOf[SAnyValue]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for AnyValue trait
  case class AnyValueAdapter(source: Rep[AnyValue])
      extends AnyValue with Def[AnyValue] {
    val selfType: Elem[AnyValue] = element[AnyValue]
    private val thisClass = classOf[AnyValue]

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAnyValue(p: Rep[AnyValue]): AnyValue = {
    if (p.rhs.isInstanceOf[AnyValue@unchecked]) p.rhs.asInstanceOf[AnyValue]
    else
      AnyValueAdapter(p)
  }

  // familyElem
  class AnyValueElem[To <: AnyValue]
    extends EntityElem[To] {
    override val liftable = LiftableAnyValue.asLiftable[SAnyValue, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[AnyValue], classOf[SAnyValue], Set(
        "dataSize"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[AnyValue].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[AnyValue] => convertAnyValue(x) }
      tryConvert(element[AnyValue], this, x, conv)
    }

    def convertAnyValue(x: Rep[AnyValue]): Rep[To] = {
      x.elem match {
        case _: AnyValueElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have AnyValueElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def anyValueElement: Elem[AnyValue] =
    cachedElem[AnyValueElem[AnyValue]]()

  implicit case object AnyValueCompanionElem extends CompanionElem[AnyValueCompanionCtor] {
    lazy val tag = weakTypeTag[AnyValueCompanionCtor]
    protected def getDefaultRep = RAnyValue
  }

  abstract class AnyValueCompanionCtor extends CompanionDef[AnyValueCompanionCtor] with AnyValueCompanion {
    def selfType = AnyValueCompanionElem
    override def toString = "AnyValue"
  }
  implicit def proxyAnyValueCompanionCtor(p: Rep[AnyValueCompanionCtor]): AnyValueCompanionCtor =
    proxyOps[AnyValueCompanionCtor](p)

  lazy val RAnyValue: Rep[AnyValueCompanionCtor] = new AnyValueCompanionCtor {
    private val thisClass = classOf[AnyValueCompanion]
  }

  object AnyValueMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AnyValueElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object AnyValueCompanionMethods {
  }
} // of object AnyValue
  registerEntityObject("AnyValue", AnyValue)

object Box extends EntityObject("Box") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SBox = special.sigma.Box
  case class BoxConst(
        constValue: SBox
      ) extends Box with LiftedConst[SBox, Box] {
    val liftable: Liftable[SBox, Box] = LiftableBox
    val selfType: Elem[Box] = liftable.eW
    private val thisClass = classOf[Box]

    // manual fix
    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        thisClass.getMethod("builder"),
        List(),
        true, isAdapterCall = false, element[SigmaDslBuilder]))
    }

    def id: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("id"),
        List(),
        true, false, element[Col[Byte]]))
    }

    def value: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("value"),
        List(),
        true, false, element[Long]))
    }

    def bytes: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("bytes"),
        List(),
        true, false, element[Col[Byte]]))
    }

    def bytesWithoutRef: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("bytesWithoutRef"),
        List(),
        true, false, element[Col[Byte]]))
    }

    def propositionBytes: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("propositionBytes"),
        List(),
        true, false, element[Col[Byte]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }

    def registers: Rep[Col[AnyValue]] = {
      asRep[Col[AnyValue]](mkMethodCall(self,
        thisClass.getMethod("registers"),
        List(),
        true, false, element[Col[AnyValue]]))
    }

    def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        thisClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        List(i, cT),
        true, false, element[WOption[T]]))
    }

    def tokens: Rep[Col[(Col[Byte], Long)]] = {
      asRep[Col[(Col[Byte], Long)]](mkMethodCall(self,
        thisClass.getMethod("tokens"),
        List(),
        true, false, element[Col[(Col[Byte], Long)]]))
    }

    def creationInfo: Rep[(Long, Col[Byte])] = {
      asRep[(Long, Col[Byte])](mkMethodCall(self,
        thisClass.getMethod("creationInfo"),
        List(),
        true, false, element[(Long, Col[Byte])]))
    }
  }

  implicit object LiftableBox
    extends Liftable[SBox, Box] {
    lazy val eW: Elem[Box] = boxElement
    lazy val sourceClassTag: ClassTag[SBox] = {
      classTag[SBox]
    }
    def lift(x: SBox): Rep[Box] = BoxConst(x)
    def unlift(w: Rep[Box]): SBox = w match {
      case Def(BoxConst(x: SBox))
            => x.asInstanceOf[SBox]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for Box trait
  case class BoxAdapter(source: Rep[Box])
      extends Box with Def[Box] {
    val selfType: Elem[Box] = element[Box]
    private val thisClass = classOf[Box]

    def id: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("id"),
        List(),
        true, true, element[Col[Byte]]))
    }

    def value: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Long]))
    }

    def bytes: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("bytes"),
        List(),
        true, true, element[Col[Byte]]))
    }

    def bytesWithoutRef: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("bytesWithoutRef"),
        List(),
        true, true, element[Col[Byte]]))
    }

    def propositionBytes: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("propositionBytes"),
        List(),
        true, true, element[Col[Byte]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }

    def registers: Rep[Col[AnyValue]] = {
      asRep[Col[AnyValue]](mkMethodCall(source,
        thisClass.getMethod("registers"),
        List(),
        true, true, element[Col[AnyValue]]))
    }

    def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        thisClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        List(i, cT),
        true, true, element[WOption[T]]))
    }

    def tokens: Rep[Col[(Col[Byte], Long)]] = {
      asRep[Col[(Col[Byte], Long)]](mkMethodCall(source,
        thisClass.getMethod("tokens"),
        List(),
        true, true, element[Col[(Col[Byte], Long)]]))
    }

    def creationInfo: Rep[(Long, Col[Byte])] = {
      asRep[(Long, Col[Byte])](mkMethodCall(source,
        thisClass.getMethod("creationInfo"),
        List(),
        true, true, element[(Long, Col[Byte])]))
    }

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBox(p: Rep[Box]): Box = {
    if (p.rhs.isInstanceOf[Box@unchecked]) p.rhs.asInstanceOf[Box]
    else
      BoxAdapter(p)
  }

  // familyElem
  class BoxElem[To <: Box]
    extends DslObjectElem[To] {
    override val liftable = LiftableBox.asLiftable[SBox, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Box], classOf[SBox], Set(
        "id", "value", "bytes", "bytesWithoutRef", "propositionBytes", "cost", "dataSize", "registers", "getReg", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "tokens", "creationInfo"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslObjectElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Box].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Box] => convertBox(x) }
      tryConvert(element[Box], this, x, conv)
    }

    def convertBox(x: Rep[Box]): Rep[To] = {
      x.elem match {
        case _: BoxElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have BoxElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def boxElement: Elem[Box] =
    cachedElem[BoxElem[Box]]()

  implicit case object BoxCompanionElem extends CompanionElem[BoxCompanionCtor] {
    lazy val tag = weakTypeTag[BoxCompanionCtor]
    protected def getDefaultRep = RBox
  }

  abstract class BoxCompanionCtor extends CompanionDef[BoxCompanionCtor] with BoxCompanion {
    def selfType = BoxCompanionElem
    override def toString = "Box"
  }
  implicit def proxyBoxCompanionCtor(p: Rep[BoxCompanionCtor]): BoxCompanionCtor =
    proxyOps[BoxCompanionCtor](p)

  lazy val RBox: Rep[BoxCompanionCtor] = new BoxCompanionCtor {
    private val thisClass = classOf[BoxCompanion]
  }

  object BoxMethods {
    object id {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "id" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "bytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "bytesWithoutRef" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "propositionBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "registers" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "getReg" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R0 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R0" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R1 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R1" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R2 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R2" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R3 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R3" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R4 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R4" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R5 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R5" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R6 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R6" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R7 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R7" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R8 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R8" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R9 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R9" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object tokens {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "tokens" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object creationInfo {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "creationInfo" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object BoxCompanionMethods {
  }
} // of object Box
  registerEntityObject("Box", Box)

object AvlTree extends EntityObject("AvlTree") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SAvlTree = special.sigma.AvlTree
  case class AvlTreeConst(
        constValue: SAvlTree
      ) extends AvlTree with LiftedConst[SAvlTree, AvlTree] {
    val liftable: Liftable[SAvlTree, AvlTree] = LiftableAvlTree
    val selfType: Elem[AvlTree] = liftable.eW
    private val thisClass = classOf[AvlTree]

    // manual fix
    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        thisClass.getMethod("builder"),
        List(),
        true, isAdapterCall = false, element[SigmaDslBuilder]))
    }

    def startingDigest: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("startingDigest"),
        List(),
        true, false, element[Col[Byte]]))
    }

    def keyLength: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("keyLength"),
        List(),
        true, false, element[Int]))
    }

    def valueLengthOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(self,
        thisClass.getMethod("valueLengthOpt"),
        List(),
        true, false, element[WOption[Int]]))
    }

    def maxNumOperations: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(self,
        thisClass.getMethod("maxNumOperations"),
        List(),
        true, false, element[WOption[Int]]))
    }

    def maxDeletes: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(self,
        thisClass.getMethod("maxDeletes"),
        List(),
        true, false, element[WOption[Int]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }

  implicit object LiftableAvlTree
    extends Liftable[SAvlTree, AvlTree] {
    lazy val eW: Elem[AvlTree] = avlTreeElement
    lazy val sourceClassTag: ClassTag[SAvlTree] = {
      classTag[SAvlTree]
    }
    def lift(x: SAvlTree): Rep[AvlTree] = AvlTreeConst(x)
    def unlift(w: Rep[AvlTree]): SAvlTree = w match {
      case Def(AvlTreeConst(x: SAvlTree))
            => x.asInstanceOf[SAvlTree]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for AvlTree trait
  case class AvlTreeAdapter(source: Rep[AvlTree])
      extends AvlTree with Def[AvlTree] {
    val selfType: Elem[AvlTree] = element[AvlTree]
    private val thisClass = classOf[AvlTree]

    def startingDigest: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("startingDigest"),
        List(),
        true, true, element[Col[Byte]]))
    }

    def keyLength: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("keyLength"),
        List(),
        true, true, element[Int]))
    }

    def valueLengthOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        thisClass.getMethod("valueLengthOpt"),
        List(),
        true, true, element[WOption[Int]]))
    }

    def maxNumOperations: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        thisClass.getMethod("maxNumOperations"),
        List(),
        true, true, element[WOption[Int]]))
    }

    def maxDeletes: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        thisClass.getMethod("maxDeletes"),
        List(),
        true, true, element[WOption[Int]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAvlTree(p: Rep[AvlTree]): AvlTree = {
    if (p.rhs.isInstanceOf[AvlTree@unchecked]) p.rhs.asInstanceOf[AvlTree]
    else
      AvlTreeAdapter(p)
  }

  // familyElem
  class AvlTreeElem[To <: AvlTree]
    extends DslObjectElem[To] {
    override val liftable = LiftableAvlTree.asLiftable[SAvlTree, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[AvlTree], classOf[SAvlTree], Set(
        "startingDigest", "keyLength", "valueLengthOpt", "maxNumOperations", "maxDeletes", "cost", "dataSize"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslObjectElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[AvlTree].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[AvlTree] => convertAvlTree(x) }
      tryConvert(element[AvlTree], this, x, conv)
    }

    def convertAvlTree(x: Rep[AvlTree]): Rep[To] = {
      x.elem match {
        case _: AvlTreeElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have AvlTreeElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def avlTreeElement: Elem[AvlTree] =
    cachedElem[AvlTreeElem[AvlTree]]()

  implicit case object AvlTreeCompanionElem extends CompanionElem[AvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[AvlTreeCompanionCtor]
    protected def getDefaultRep = RAvlTree
  }

  abstract class AvlTreeCompanionCtor extends CompanionDef[AvlTreeCompanionCtor] with AvlTreeCompanion {
    def selfType = AvlTreeCompanionElem
    override def toString = "AvlTree"
  }
  implicit def proxyAvlTreeCompanionCtor(p: Rep[AvlTreeCompanionCtor]): AvlTreeCompanionCtor =
    proxyOps[AvlTreeCompanionCtor](p)

  lazy val RAvlTree: Rep[AvlTreeCompanionCtor] = new AvlTreeCompanionCtor {
    private val thisClass = classOf[AvlTreeCompanion]
  }

  object AvlTreeMethods {
    object startingDigest {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "startingDigest" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object keyLength {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "keyLength" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueLengthOpt {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "valueLengthOpt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object maxNumOperations {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "maxNumOperations" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object maxDeletes {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "maxDeletes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object AvlTreeCompanionMethods {
  }
} // of object AvlTree
  registerEntityObject("AvlTree", AvlTree)

object Context extends EntityObject("Context") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SContext = special.sigma.Context
  case class ContextConst(
        constValue: SContext
      ) extends Context with LiftedConst[SContext, Context] {
    val liftable: Liftable[SContext, Context] = LiftableContext
    val selfType: Elem[Context] = liftable.eW
    private val thisClass = classOf[Context]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        thisClass.getMethod("builder"),
        List(),
        true, false, element[SigmaDslBuilder]))
    }

    def OUTPUTS: Rep[Col[Box]] = {
      asRep[Col[Box]](mkMethodCall(self,
        thisClass.getMethod("OUTPUTS"),
        List(),
        true, false, element[Col[Box]]))
    }

    def INPUTS: Rep[Col[Box]] = {
      asRep[Col[Box]](mkMethodCall(self,
        thisClass.getMethod("INPUTS"),
        List(),
        true, false, element[Col[Box]]))
    }

    def HEIGHT: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("HEIGHT"),
        List(),
        true, false, element[Long]))
    }

    def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(self,
        thisClass.getMethod("SELF"),
        List(),
        true, false, element[Box]))
    }

    def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        thisClass.getMethod("LastBlockUtxoRootHash"),
        List(),
        true, false, element[AvlTree]))
    }

    def MinerPubKey: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("MinerPubKey"),
        List(),
        true, false, element[Col[Byte]]))
    }

    def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        thisClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, element[WOption[T]]))
    }

    def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[T](mkMethodCall(self,
        thisClass.getMethod("getConstant", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, element[T]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }

  implicit object LiftableContext
    extends Liftable[SContext, Context] {
    lazy val eW: Elem[Context] = contextElement
    lazy val sourceClassTag: ClassTag[SContext] = {
      classTag[SContext]
    }
    def lift(x: SContext): Rep[Context] = ContextConst(x)
    def unlift(w: Rep[Context]): SContext = w match {
      case Def(ContextConst(x: SContext))
            => x.asInstanceOf[SContext]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for Context trait
  case class ContextAdapter(source: Rep[Context])
      extends Context with Def[Context] {
    val selfType: Elem[Context] = element[Context]
    private val thisClass = classOf[Context]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def OUTPUTS: Rep[Col[Box]] = {
      asRep[Col[Box]](mkMethodCall(source,
        thisClass.getMethod("OUTPUTS"),
        List(),
        true, true, element[Col[Box]]))
    }

    def INPUTS: Rep[Col[Box]] = {
      asRep[Col[Box]](mkMethodCall(source,
        thisClass.getMethod("INPUTS"),
        List(),
        true, true, element[Col[Box]]))
    }

    def HEIGHT: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("HEIGHT"),
        List(),
        true, true, element[Long]))
    }

    def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(source,
        thisClass.getMethod("SELF"),
        List(),
        true, true, element[Box]))
    }

    def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("LastBlockUtxoRootHash"),
        List(),
        true, true, element[AvlTree]))
    }

    def MinerPubKey: Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("MinerPubKey"),
        List(),
        true, true, element[Col[Byte]]))
    }

    def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        thisClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, true, element[WOption[T]]))
    }

    def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[T](mkMethodCall(source,
        thisClass.getMethod("getConstant", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, true, element[T]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyContext(p: Rep[Context]): Context = {
    if (p.rhs.isInstanceOf[Context@unchecked]) p.rhs.asInstanceOf[Context]
    else
      ContextAdapter(p)
  }

  // familyElem
  class ContextElem[To <: Context]
    extends EntityElem[To] {
    override val liftable = LiftableContext.asLiftable[SContext, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Context], classOf[SContext], Set(
        "builder", "OUTPUTS", "INPUTS", "HEIGHT", "SELF", "LastBlockUtxoRootHash", "MinerPubKey", "getVar", "getConstant", "cost", "dataSize"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Context].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Context] => convertContext(x) }
      tryConvert(element[Context], this, x, conv)
    }

    def convertContext(x: Rep[Context]): Rep[To] = {
      x.elem match {
        case _: ContextElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have ContextElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def contextElement: Elem[Context] =
    cachedElem[ContextElem[Context]]()

  implicit case object ContextCompanionElem extends CompanionElem[ContextCompanionCtor] {
    lazy val tag = weakTypeTag[ContextCompanionCtor]
    protected def getDefaultRep = RContext
  }

  abstract class ContextCompanionCtor extends CompanionDef[ContextCompanionCtor] with ContextCompanion {
    def selfType = ContextCompanionElem
    override def toString = "Context"
  }
  implicit def proxyContextCompanionCtor(p: Rep[ContextCompanionCtor]): ContextCompanionCtor =
    proxyOps[ContextCompanionCtor](p)

  lazy val RContext: Rep[ContextCompanionCtor] = new ContextCompanionCtor {
    private val thisClass = classOf[ContextCompanion]
  }

  object ContextMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "OUTPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "INPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "HEIGHT" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "SELF" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "LastBlockUtxoRootHash" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object MinerPubKey {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "MinerPubKey" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "getVar" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getConstant {
      def unapply(d: Def[_]): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "getConstant" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ContextCompanionMethods {
  }
} // of object Context
  registerEntityObject("Context", Context)

object SigmaContract extends EntityObject("SigmaContract") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaContract = special.sigma.SigmaContract
  case class SigmaContractConst(
        constValue: SSigmaContract
      ) extends SigmaContract with LiftedConst[SSigmaContract, SigmaContract] {
    val liftable: Liftable[SSigmaContract, SigmaContract] = LiftableSigmaContract
    val selfType: Elem[SigmaContract] = liftable.eW
    private val thisClass = classOf[SigmaContract]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        thisClass.getMethod("builder"),
        List(),
        true, false, element[SigmaDslBuilder]))
    }

    def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("canOpen", classOf[Sym]),
        List(ctx),
        true, false, element[Boolean]))
    }
  }

  implicit object LiftableSigmaContract
    extends Liftable[SSigmaContract, SigmaContract] {
    lazy val eW: Elem[SigmaContract] = sigmaContractElement
    lazy val sourceClassTag: ClassTag[SSigmaContract] = {
      classTag[SSigmaContract]
    }
    def lift(x: SSigmaContract): Rep[SigmaContract] = SigmaContractConst(x)
    def unlift(w: Rep[SigmaContract]): SSigmaContract = w match {
      case Def(SigmaContractConst(x: SSigmaContract))
            => x.asInstanceOf[SSigmaContract]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for SigmaContract trait
  case class SigmaContractAdapter(source: Rep[SigmaContract])
      extends SigmaContract with Def[SigmaContract] {
    val selfType: Elem[SigmaContract] = element[SigmaContract]
    private val thisClass = classOf[SigmaContract]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("canOpen", classOf[Sym]),
        List(ctx),
        true, true, element[Boolean]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaContract(p: Rep[SigmaContract]): SigmaContract = {
    if (p.rhs.isInstanceOf[SigmaContract@unchecked]) p.rhs.asInstanceOf[SigmaContract]
    else
      SigmaContractAdapter(p)
  }

  // familyElem
  class SigmaContractElem[To <: SigmaContract]
    extends EntityElem[To] {
    override val liftable = LiftableSigmaContract.asLiftable[SSigmaContract, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaContract], classOf[SSigmaContract], Set(
        "builder", "Collection", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "proveDlog", "proveDHTuple", "isMember", "treeLookup", "treeModifications", "groupGenerator", "exponentiate", "canOpen", "asFunction"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaContract].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaContract] => convertSigmaContract(x) }
      tryConvert(element[SigmaContract], this, x, conv)
    }

    def convertSigmaContract(x: Rep[SigmaContract]): Rep[To] = {
      x.elem match {
        case _: SigmaContractElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaContractElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaContractElement: Elem[SigmaContract] =
    cachedElem[SigmaContractElem[SigmaContract]]()

  implicit case object SigmaContractCompanionElem extends CompanionElem[SigmaContractCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaContractCompanionCtor]
    protected def getDefaultRep = RSigmaContract
  }

  abstract class SigmaContractCompanionCtor extends CompanionDef[SigmaContractCompanionCtor] with SigmaContractCompanion {
    def selfType = SigmaContractCompanionElem
    override def toString = "SigmaContract"
  }
  implicit def proxySigmaContractCompanionCtor(p: Rep[SigmaContractCompanionCtor]): SigmaContractCompanionCtor =
    proxyOps[SigmaContractCompanionCtor](p)

  lazy val RSigmaContract: Rep[SigmaContractCompanionCtor] = new SigmaContractCompanionCtor {
    private val thisClass = classOf[SigmaContractCompanion]
  }

  object SigmaContractMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Collection {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "Collection" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "verifyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Int], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "atLeast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Int], Rep[Col[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Int], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "PubKey" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "sigmaProp" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "blake2b256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "sha256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "byteArrayToBigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "longToByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "proveDlog" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "proveDHTuple" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isMember {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "isMember" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object treeLookup {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "treeLookup" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object treeModifications {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "treeModifications" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Nullable[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "groupGenerator" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exponentiate {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "exponentiate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[WECPoint], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object canOpen {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Context])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "canOpen" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Context])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object asFunction {
      def unapply(d: Def[_]): Nullable[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "asFunction" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaContractCompanionMethods {
  }
} // of object SigmaContract
  registerEntityObject("SigmaContract", SigmaContract)

object SigmaDslBuilder extends EntityObject("SigmaDslBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaDslBuilder = special.sigma.SigmaDslBuilder
  case class SigmaDslBuilderConst(
        constValue: SSigmaDslBuilder
      ) extends SigmaDslBuilder with LiftedConst[SSigmaDslBuilder, SigmaDslBuilder] {
    val liftable: Liftable[SSigmaDslBuilder, SigmaDslBuilder] = LiftableSigmaDslBuilder
    val selfType: Elem[SigmaDslBuilder] = liftable.eW
    private val thisClass = classOf[SigmaDslBuilder]

    def Cols: Rep[ColBuilder] = {
      asRep[ColBuilder](mkMethodCall(self,
        thisClass.getMethod("Cols"),
        List(),
        true, false, element[ColBuilder]))
    }

    def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(self,
        thisClass.getMethod("Monoids"),
        List(),
        true, false, element[MonoidBuilder]))
    }

    def Costing: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(self,
        thisClass.getMethod("Costing"),
        List(),
        true, false, element[CostedBuilder]))
    }

    def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(self,
        thisClass.getMethod("CostModel"),
        List(),
        true, false, element[CostModel]))
    }

    def costBoxes(bs: Rep[Col[Box]]): Rep[CostedCol[Box]] = {
      asRep[CostedCol[Box]](mkMethodCall(self,
        thisClass.getMethod("costBoxes", classOf[Sym]),
        List(bs),
        true, false, element[CostedCol[Box]]))
    }

    def costColWithConstSizedItem[T](xs: Rep[Col[T]], len: Rep[Int], itemSize: Rep[Long]): Rep[CostedCol[T]] = {
      implicit val eT = xs.eA
      asRep[CostedCol[T]](mkMethodCall(self,
        thisClass.getMethod("costColWithConstSizedItem", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(xs, len, itemSize),
        true, false, element[CostedCol[T]]))
    }

    def costOption[T](opt: Rep[WOption[T]], opCost: Rep[Int]): Rep[CostedOption[T]] = {
      implicit val eT = opt.eA
      asRep[CostedOption[T]](mkMethodCall(self,
        thisClass.getMethod("costOption", classOf[Sym], classOf[Sym]),
        List(opt, opCost),
        true, false, element[CostedOption[T]]))
    }

    def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("verifyZK", classOf[Sym]),
        List(cond),
        true, false, element[Boolean]))
    }

    def atLeast(bound: Rep[Int], props: Rep[Col[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        List(bound, props),
        true, false, element[SigmaProp]))
    }

    def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("allOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    def allZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("allZK", classOf[Sym]),
        List(conditions),
        true, false, element[SigmaProp]))
    }

    def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("anyOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    def anyZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("anyZK", classOf[Sym]),
        List(conditions),
        true, false, element[SigmaProp]))
    }

    def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("PubKey", classOf[Sym]),
        List(base64String),
        true, false, element[SigmaProp]))
    }

    def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("sigmaProp", classOf[Sym]),
        List(b),
        true, false, element[SigmaProp]))
    }

    def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("blake2b256", classOf[Sym]),
        List(bytes),
        true, false, element[Col[Byte]]))
    }

    def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("sha256", classOf[Sym]),
        List(bytes),
        true, false, element[Col[Byte]]))
    }

    def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        List(bytes),
        true, false, element[WBigInteger]))
    }

    def longToByteArray(l: Rep[Long]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("longToByteArray", classOf[Sym]),
        List(l),
        true, false, element[Col[Byte]]))
    }

    def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("proveDlog", classOf[Sym]),
        List(g),
        true, false, element[SigmaProp]))
    }

    def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(g, h, u, v),
        true, false, element[SigmaProp]))
    }

    def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isMember", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, key, proof),
        true, false, element[Boolean]))
    }

    def treeLookup(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = {
      asRep[WOption[Col[Byte]]](mkMethodCall(self,
        thisClass.getMethod("treeLookup", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, key, proof),
        true, false, element[WOption[Col[Byte]]]))
    }

    def treeModifications(tree: Rep[AvlTree], operations: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = {
      asRep[WOption[Col[Byte]]](mkMethodCall(self,
        thisClass.getMethod("treeModifications", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, operations, proof),
        true, false, element[WOption[Col[Byte]]]))
    }

    def groupGenerator: Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        thisClass.getMethod("groupGenerator"),
        List(),
        true, false, element[WECPoint]))
    }

    def exponentiate(base: Rep[WECPoint], exponent: Rep[WBigInteger]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        thisClass.getMethod("exponentiate", classOf[Sym], classOf[Sym]),
        List(base, exponent),
        true, false, element[WECPoint]))
    }

    def substConstants[T](scriptBytes: Rep[Col[Byte]], positions: Rep[Col[Int]], newValues: Rep[Col[T]])(implicit cT: Elem[T]): Rep[Col[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        List(scriptBytes, positions, newValues, cT),
        true, false, element[Col[Byte]]))
    }
  }

  implicit object LiftableSigmaDslBuilder
    extends Liftable[SSigmaDslBuilder, SigmaDslBuilder] {
    lazy val eW: Elem[SigmaDslBuilder] = sigmaDslBuilderElement
    lazy val sourceClassTag: ClassTag[SSigmaDslBuilder] = {
      classTag[SSigmaDslBuilder]
    }
    def lift(x: SSigmaDslBuilder): Rep[SigmaDslBuilder] = SigmaDslBuilderConst(x)
    def unlift(w: Rep[SigmaDslBuilder]): SSigmaDslBuilder = w match {
      case Def(SigmaDslBuilderConst(x: SSigmaDslBuilder))
            => x.asInstanceOf[SSigmaDslBuilder]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for SigmaDslBuilder trait
  case class SigmaDslBuilderAdapter(source: Rep[SigmaDslBuilder])
      extends SigmaDslBuilder with Def[SigmaDslBuilder] {
    val selfType: Elem[SigmaDslBuilder] = element[SigmaDslBuilder]
    private val thisClass = classOf[SigmaDslBuilder]

    def Cols: Rep[ColBuilder] = {
      asRep[ColBuilder](mkMethodCall(source,
        thisClass.getMethod("Cols"),
        List(),
        true, true, element[ColBuilder]))
    }

    def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        thisClass.getMethod("Monoids"),
        List(),
        true, true, element[MonoidBuilder]))
    }

    def Costing: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("Costing"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(source,
        thisClass.getMethod("CostModel"),
        List(),
        true, true, element[CostModel]))
    }

    def costBoxes(bs: Rep[Col[Box]]): Rep[CostedCol[Box]] = {
      asRep[CostedCol[Box]](mkMethodCall(source,
        thisClass.getMethod("costBoxes", classOf[Sym]),
        List(bs),
        true, true, element[CostedCol[Box]]))
    }

    def costColWithConstSizedItem[T](xs: Rep[Col[T]], len: Rep[Int], itemSize: Rep[Long]): Rep[CostedCol[T]] = {
      implicit val eT = xs.eA
      asRep[CostedCol[T]](mkMethodCall(source,
        thisClass.getMethod("costColWithConstSizedItem", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(xs, len, itemSize),
        true, true, element[CostedCol[T]]))
    }

    def costOption[T](opt: Rep[WOption[T]], opCost: Rep[Int]): Rep[CostedOption[T]] = {
      implicit val eT = opt.eA
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("costOption", classOf[Sym], classOf[Sym]),
        List(opt, opCost),
        true, true, element[CostedOption[T]]))
    }

    def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("verifyZK", classOf[Sym]),
        List(cond),
        true, true, element[Boolean]))
    }

    def atLeast(bound: Rep[Int], props: Rep[Col[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        List(bound, props),
        true, true, element[SigmaProp]))
    }

    def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("allOf", classOf[Sym]),
        List(conditions),
        true, true, element[Boolean]))
    }

    def allZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("allZK", classOf[Sym]),
        List(conditions),
        true, true, element[SigmaProp]))
    }

    def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("anyOf", classOf[Sym]),
        List(conditions),
        true, true, element[Boolean]))
    }

    def anyZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("anyZK", classOf[Sym]),
        List(conditions),
        true, true, element[SigmaProp]))
    }

    def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("PubKey", classOf[Sym]),
        List(base64String),
        true, true, element[SigmaProp]))
    }

    def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("sigmaProp", classOf[Sym]),
        List(b),
        true, true, element[SigmaProp]))
    }

    def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("blake2b256", classOf[Sym]),
        List(bytes),
        true, true, element[Col[Byte]]))
    }

    def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("sha256", classOf[Sym]),
        List(bytes),
        true, true, element[Col[Byte]]))
    }

    def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        List(bytes),
        true, true, element[WBigInteger]))
    }

    def longToByteArray(l: Rep[Long]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("longToByteArray", classOf[Sym]),
        List(l),
        true, true, element[Col[Byte]]))
    }

    def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("proveDlog", classOf[Sym]),
        List(g),
        true, true, element[SigmaProp]))
    }

    def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(g, h, u, v),
        true, true, element[SigmaProp]))
    }

    def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isMember", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, key, proof),
        true, true, element[Boolean]))
    }

    def treeLookup(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = {
      asRep[WOption[Col[Byte]]](mkMethodCall(source,
        thisClass.getMethod("treeLookup", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, key, proof),
        true, true, element[WOption[Col[Byte]]]))
    }

    def treeModifications(tree: Rep[AvlTree], operations: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = {
      asRep[WOption[Col[Byte]]](mkMethodCall(source,
        thisClass.getMethod("treeModifications", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, operations, proof),
        true, true, element[WOption[Col[Byte]]]))
    }

    def groupGenerator: Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(source,
        thisClass.getMethod("groupGenerator"),
        List(),
        true, true, element[WECPoint]))
    }

    def exponentiate(base: Rep[WECPoint], exponent: Rep[WBigInteger]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(source,
        thisClass.getMethod("exponentiate", classOf[Sym], classOf[Sym]),
        List(base, exponent),
        true, true, element[WECPoint]))
    }

    def substConstants[T](scriptBytes: Rep[Col[Byte]], positions: Rep[Col[Int]], newValues: Rep[Col[T]])(implicit cT: Elem[T]): Rep[Col[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        List(scriptBytes, positions, newValues, cT),
        true, true, element[Col[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaDslBuilder(p: Rep[SigmaDslBuilder]): SigmaDslBuilder = {
    if (p.rhs.isInstanceOf[SigmaDslBuilder@unchecked]) p.rhs.asInstanceOf[SigmaDslBuilder]
    else
      SigmaDslBuilderAdapter(p)
  }

  // familyElem
  class SigmaDslBuilderElem[To <: SigmaDslBuilder]
    extends DslBuilderElem[To] {
    override val liftable = LiftableSigmaDslBuilder.asLiftable[SSigmaDslBuilder, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaDslBuilder], classOf[SSigmaDslBuilder], Set(
        "Cols", "Monoids", "Costing", "CostModel", "costBoxes", "costColWithConstSizedItem", "costOption", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "proveDlog", "proveDHTuple", "isMember", "treeLookup", "treeModifications", "groupGenerator", "exponentiate", "substConstants"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(dslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaDslBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaDslBuilder] => convertSigmaDslBuilder(x) }
      tryConvert(element[SigmaDslBuilder], this, x, conv)
    }

    def convertSigmaDslBuilder(x: Rep[SigmaDslBuilder]): Rep[To] = {
      x.elem match {
        case _: SigmaDslBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaDslBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sigmaDslBuilderElement: Elem[SigmaDslBuilder] =
    cachedElem[SigmaDslBuilderElem[SigmaDslBuilder]]()

  implicit case object SigmaDslBuilderCompanionElem extends CompanionElem[SigmaDslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaDslBuilderCompanionCtor]
    protected def getDefaultRep = RSigmaDslBuilder
  }

  abstract class SigmaDslBuilderCompanionCtor extends CompanionDef[SigmaDslBuilderCompanionCtor] with SigmaDslBuilderCompanion {
    def selfType = SigmaDslBuilderCompanionElem
    override def toString = "SigmaDslBuilder"
  }
  implicit def proxySigmaDslBuilderCompanionCtor(p: Rep[SigmaDslBuilderCompanionCtor]): SigmaDslBuilderCompanionCtor =
    proxyOps[SigmaDslBuilderCompanionCtor](p)

  lazy val RSigmaDslBuilder: Rep[SigmaDslBuilderCompanionCtor] = new SigmaDslBuilderCompanionCtor {
    private val thisClass = classOf[SigmaDslBuilderCompanion]
  }

  object SigmaDslBuilderMethods {
    object Cols {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Cols" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Monoids {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Monoids" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Costing {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Costing" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object CostModel {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "CostModel" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costBoxes {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Box]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "costBoxes" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Box]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Box]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costColWithConstSizedItem {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[T]], Rep[Int], Rep[Long]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "costColWithConstSizedItem" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[T]], Rep[Int], Rep[Long]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[T]], Rep[Int], Rep[Long]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costOption {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[WOption[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "costOption" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[WOption[T]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[WOption[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "verifyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "atLeast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "allOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "allZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "anyOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "anyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "PubKey" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "sigmaProp" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "blake2b256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "sha256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "byteArrayToBigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "longToByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "proveDlog" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "proveDHTuple" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isMember {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "isMember" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object treeLookup {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "treeLookup" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object treeModifications {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "treeModifications" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[AvlTree], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "groupGenerator" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exponentiate {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "exponentiate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object substConstants {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]], Rep[Col[Int]], Rep[Col[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "substConstants" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]], Rep[Col[Int]], Rep[Col[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Col[Byte]], Rep[Col[Int]], Rep[Col[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaDslBuilderCompanionMethods {
  }
} // of object SigmaDslBuilder
  registerEntityObject("SigmaDslBuilder", SigmaDslBuilder)

  registerModule(SigmaDslModule)
}

object SigmaDslModule extends scalan.ModuleInfo("special.sigma", "SigmaDsl")
}

trait SigmaDslModule extends special.sigma.impl.SigmaDslDefs {self: SigmaLibrary =>}
