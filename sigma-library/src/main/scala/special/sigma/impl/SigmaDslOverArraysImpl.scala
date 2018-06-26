package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslOverArraysDefs extends scalan.Scalan with SigmaDslOverArrays {
  self: SigmaLibrary =>

  // entityProxy: single proxy for each type family
  implicit def proxyDefaultSigma(p: Rep[DefaultSigma]): DefaultSigma = {
    proxyOps[DefaultSigma](p)(scala.reflect.classTag[DefaultSigma])
  }

  // familyElem
  class DefaultSigmaElem[To <: DefaultSigma]
    extends SigmaElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DefaultSigma].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DefaultSigma] => convertDefaultSigma(x) }
      tryConvert(element[DefaultSigma], this, x, conv)
    }

    def convertDefaultSigma(x: Rep[DefaultSigma]): Rep[To] = {
      x.elem match {
        case _: DefaultSigmaElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DefaultSigmaElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def defaultSigmaElement: Elem[DefaultSigma] =
    cachedElem[DefaultSigmaElem[DefaultSigma]]()

  implicit case object DefaultSigmaCompanionElem extends CompanionElem[DefaultSigmaCompanionCtor] {
    lazy val tag = weakTypeTag[DefaultSigmaCompanionCtor]
    protected def getDefaultRep = DefaultSigma
  }

  abstract class DefaultSigmaCompanionCtor extends CompanionDef[DefaultSigmaCompanionCtor] with DefaultSigmaCompanion {
    def selfType = DefaultSigmaCompanionElem
    override def toString = "DefaultSigma"
  }
  implicit def proxyDefaultSigmaCompanionCtor(p: Rep[DefaultSigmaCompanionCtor]): DefaultSigmaCompanionCtor =
    proxyOps[DefaultSigmaCompanionCtor](p)

  lazy val DefaultSigma: Rep[DefaultSigmaCompanionCtor] = new DefaultSigmaCompanionCtor {
  }

  object DefaultSigmaMethods {
    object and_sigma_&& {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Sigma])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Sigma])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Sigma])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Sigma])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Sigma])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Sigma])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Option[(Rep[DefaultSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[DefaultSigma], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DefaultSigmaCompanionMethods {
  }

  // entityProxy: single proxy for each type family
  implicit def proxyDefaultContract(p: Rep[DefaultContract]): DefaultContract = {
    proxyOps[DefaultContract](p)(scala.reflect.classTag[DefaultContract])
  }

  // familyElem
  class DefaultContractElem[To <: DefaultContract]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DefaultContract].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DefaultContract] => convertDefaultContract(x) }
      tryConvert(element[DefaultContract], this, x, conv)
    }

    def convertDefaultContract(x: Rep[DefaultContract]): Rep[To] = {
      x.elem match {
        case _: DefaultContractElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have DefaultContractElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def defaultContractElement: Elem[DefaultContract] =
    cachedElem[DefaultContractElem[DefaultContract]]()

  implicit case object DefaultContractCompanionElem extends CompanionElem[DefaultContractCompanionCtor] {
    lazy val tag = weakTypeTag[DefaultContractCompanionCtor]
    protected def getDefaultRep = DefaultContract
  }

  abstract class DefaultContractCompanionCtor extends CompanionDef[DefaultContractCompanionCtor] with DefaultContractCompanion {
    def selfType = DefaultContractCompanionElem
    override def toString = "DefaultContract"
  }
  implicit def proxyDefaultContractCompanionCtor(p: Rep[DefaultContractCompanionCtor]): DefaultContractCompanionCtor =
    proxyOps[DefaultContractCompanionCtor](p)

  lazy val DefaultContract: Rep[DefaultContractCompanionCtor] = new DefaultContractCompanionCtor {
  }

  object DefaultContractMethods {
    object verify {
      def unapply(d: Def[_]): Option[(Rep[DefaultContract], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, Seq(cond, _*), _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "verify" =>
          Some((receiver, cond)).asInstanceOf[Option[(Rep[DefaultContract], Rep[Boolean])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultContract], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Option[(Rep[DefaultContract], Rep[Sigma])] = d match {
        case MethodCall(receiver, method, Seq(proof, _*), _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "verifyZK" =>
          Some((receiver, proof)).asInstanceOf[Option[(Rep[DefaultContract], Rep[Sigma])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultContract], Rep[Sigma])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Option[(Rep[DefaultContract], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "allOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[DefaultContract], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultContract], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Option[(Rep[DefaultContract], Rep[Col[Boolean]])] = d match {
        case MethodCall(receiver, method, Seq(conditions, _*), _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "anyOf" =>
          Some((receiver, conditions)).asInstanceOf[Option[(Rep[DefaultContract], Rep[Col[Boolean]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultContract], Rep[Col[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Option[(Rep[DefaultContract], Rep[Col[Sigma]])] = d match {
        case MethodCall(receiver, method, Seq(proofs, _*), _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "allZK" =>
          Some((receiver, proofs)).asInstanceOf[Option[(Rep[DefaultContract], Rep[Col[Sigma]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultContract], Rep[Col[Sigma]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Option[(Rep[DefaultContract], Rep[Col[Sigma]])] = d match {
        case MethodCall(receiver, method, Seq(proofs, _*), _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "anyZK" =>
          Some((receiver, proofs)).asInstanceOf[Option[(Rep[DefaultContract], Rep[Col[Sigma]])]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[DefaultContract], Rep[Col[Sigma]])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object DefaultContractCompanionMethods {
  }

  case class ContextOverArraysCtor
      (override val inputs: Rep[WArray[Box]], override val outputs: Rep[WArray[Box]], override val HEIGHT: Rep[Long])
    extends ContextOverArrays(inputs, outputs, HEIGHT) with Def[ContextOverArrays] {
    lazy val selfType = element[ContextOverArrays]
  }
  // elem for concrete class
  class ContextOverArraysElem(val iso: Iso[ContextOverArraysData, ContextOverArrays])
    extends ContextElem[ContextOverArrays]
    with ConcreteElem[ContextOverArraysData, ContextOverArrays] {
    override lazy val parent: Option[Elem[_]] = Some(contextElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertContext(x: Rep[Context]) = // Converter is not generated by meta
!!!("Cannot convert from Context to ContextOverArrays: missing fields List(inputs, outputs)")
    override def getDefaultRep = ContextOverArrays(element[WArray[Box]].defaultRepValue, element[WArray[Box]].defaultRepValue, 0l)
    override lazy val tag = {
      weakTypeTag[ContextOverArrays]
    }
  }

  // state representation type
  type ContextOverArraysData = (WArray[Box], (WArray[Box], Long))

  // 3) Iso for concrete class
  class ContextOverArraysIso
    extends EntityIso[ContextOverArraysData, ContextOverArrays] with Def[ContextOverArraysIso] {
    private lazy val _safeFrom = fun { p: Rep[ContextOverArrays] => (p.inputs, p.outputs, p.HEIGHT) }
    override def from(p: Rep[ContextOverArrays]) =
      tryConvert[ContextOverArrays, (WArray[Box], (WArray[Box], Long))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WArray[Box], (WArray[Box], Long))]) = {
      val Pair(inputs, Pair(outputs, _HEIGHT)) = p
      ContextOverArrays(inputs, outputs, _HEIGHT)
    }
    lazy val eFrom = pairElement(element[WArray[Box]], pairElement(element[WArray[Box]], element[Long]))
    lazy val eTo = new ContextOverArraysElem(self)
    lazy val selfType = new ContextOverArraysIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ContextOverArraysIsoElem() extends Elem[ContextOverArraysIso] {
    def getDefaultRep = reifyObject(new ContextOverArraysIso())
    lazy val tag = {
      weakTypeTag[ContextOverArraysIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ContextOverArraysCompanionCtor extends CompanionDef[ContextOverArraysCompanionCtor] with ContextOverArraysCompanion {
    def selfType = ContextOverArraysCompanionElem
    override def toString = "ContextOverArraysCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ContextOverArraysData]): Rep[ContextOverArrays] = {
      isoContextOverArrays.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], HEIGHT: Rep[Long]): Rep[ContextOverArrays] =
      mkContextOverArrays(inputs, outputs, HEIGHT)

    def unapply(p: Rep[Context]) = unmkContextOverArrays(p)
  }
  lazy val ContextOverArraysRep: Rep[ContextOverArraysCompanionCtor] = new ContextOverArraysCompanionCtor
  lazy val ContextOverArrays: ContextOverArraysCompanionCtor = proxyContextOverArraysCompanion(ContextOverArraysRep)
  implicit def proxyContextOverArraysCompanion(p: Rep[ContextOverArraysCompanionCtor]): ContextOverArraysCompanionCtor = {
    proxyOps[ContextOverArraysCompanionCtor](p)
  }

  implicit case object ContextOverArraysCompanionElem extends CompanionElem[ContextOverArraysCompanionCtor] {
    lazy val tag = weakTypeTag[ContextOverArraysCompanionCtor]
    protected def getDefaultRep = ContextOverArraysRep
  }

  implicit def proxyContextOverArrays(p: Rep[ContextOverArrays]): ContextOverArrays =
    proxyOps[ContextOverArrays](p)

  implicit class ExtendedContextOverArrays(p: Rep[ContextOverArrays]) {
    def toData: Rep[ContextOverArraysData] = {
      isoContextOverArrays.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoContextOverArrays: Iso[ContextOverArraysData, ContextOverArrays] =
    reifyObject(new ContextOverArraysIso())

  case class ContextOverArrayBuilderCtor
      ()
    extends ContextOverArrayBuilder() with Def[ContextOverArrayBuilder] {
    lazy val selfType = element[ContextOverArrayBuilder]
  }
  // elem for concrete class
  class ContextOverArrayBuilderElem(val iso: Iso[ContextOverArrayBuilderData, ContextOverArrayBuilder])
    extends ContextBuilderElem[ContextOverArrayBuilder]
    with ConcreteElem[ContextOverArrayBuilderData, ContextOverArrayBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(contextBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertContextBuilder(x: Rep[ContextBuilder]) = ContextOverArrayBuilder()
    override def getDefaultRep = ContextOverArrayBuilder()
    override lazy val tag = {
      weakTypeTag[ContextOverArrayBuilder]
    }
  }

  // state representation type
  type ContextOverArrayBuilderData = Unit

  // 3) Iso for concrete class
  class ContextOverArrayBuilderIso
    extends EntityIso[ContextOverArrayBuilderData, ContextOverArrayBuilder] with Def[ContextOverArrayBuilderIso] {
    private lazy val _safeFrom = fun { p: Rep[ContextOverArrayBuilder] => () }
    override def from(p: Rep[ContextOverArrayBuilder]) =
      tryConvert[ContextOverArrayBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      ContextOverArrayBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new ContextOverArrayBuilderElem(self)
    lazy val selfType = new ContextOverArrayBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ContextOverArrayBuilderIsoElem() extends Elem[ContextOverArrayBuilderIso] {
    def getDefaultRep = reifyObject(new ContextOverArrayBuilderIso())
    lazy val tag = {
      weakTypeTag[ContextOverArrayBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ContextOverArrayBuilderCompanionCtor extends CompanionDef[ContextOverArrayBuilderCompanionCtor] with ContextOverArrayBuilderCompanion {
    def selfType = ContextOverArrayBuilderCompanionElem
    override def toString = "ContextOverArrayBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ContextOverArrayBuilderData]): Rep[ContextOverArrayBuilder] = {
      isoContextOverArrayBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[ContextOverArrayBuilder] =
      mkContextOverArrayBuilder()

    def unapply(p: Rep[ContextBuilder]) = unmkContextOverArrayBuilder(p)
  }
  lazy val ContextOverArrayBuilderRep: Rep[ContextOverArrayBuilderCompanionCtor] = new ContextOverArrayBuilderCompanionCtor
  lazy val ContextOverArrayBuilder: ContextOverArrayBuilderCompanionCtor = proxyContextOverArrayBuilderCompanion(ContextOverArrayBuilderRep)
  implicit def proxyContextOverArrayBuilderCompanion(p: Rep[ContextOverArrayBuilderCompanionCtor]): ContextOverArrayBuilderCompanionCtor = {
    proxyOps[ContextOverArrayBuilderCompanionCtor](p)
  }

  implicit case object ContextOverArrayBuilderCompanionElem extends CompanionElem[ContextOverArrayBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[ContextOverArrayBuilderCompanionCtor]
    protected def getDefaultRep = ContextOverArrayBuilderRep
  }

  implicit def proxyContextOverArrayBuilder(p: Rep[ContextOverArrayBuilder]): ContextOverArrayBuilder =
    proxyOps[ContextOverArrayBuilder](p)

  implicit class ExtendedContextOverArrayBuilder(p: Rep[ContextOverArrayBuilder]) {
    def toData: Rep[ContextOverArrayBuilderData] = {
      isoContextOverArrayBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoContextOverArrayBuilder: Iso[ContextOverArrayBuilderData, ContextOverArrayBuilder] =
    reifyObject(new ContextOverArrayBuilderIso())

  case class TrivialSigmaCtor
      (override val isValid: Rep[Boolean])
    extends TrivialSigma(isValid) with Def[TrivialSigma] {
    lazy val selfType = element[TrivialSigma]
  }
  // elem for concrete class
  class TrivialSigmaElem(val iso: Iso[TrivialSigmaData, TrivialSigma])
    extends SigmaElem[TrivialSigma]
    with ConcreteElem[TrivialSigmaData, TrivialSigma] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSigma(x: Rep[Sigma]) = TrivialSigma(x.isValid)
    override def getDefaultRep = TrivialSigma(false)
    override lazy val tag = {
      weakTypeTag[TrivialSigma]
    }
  }

  // state representation type
  type TrivialSigmaData = Boolean

  // 3) Iso for concrete class
  class TrivialSigmaIso
    extends EntityIso[TrivialSigmaData, TrivialSigma] with Def[TrivialSigmaIso] {
    private lazy val _safeFrom = fun { p: Rep[TrivialSigma] => p.isValid }
    override def from(p: Rep[TrivialSigma]) =
      tryConvert[TrivialSigma, Boolean](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Boolean]) = {
      val isValid = p
      TrivialSigma(isValid)
    }
    lazy val eFrom = element[Boolean]
    lazy val eTo = new TrivialSigmaElem(self)
    lazy val selfType = new TrivialSigmaIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TrivialSigmaIsoElem() extends Elem[TrivialSigmaIso] {
    def getDefaultRep = reifyObject(new TrivialSigmaIso())
    lazy val tag = {
      weakTypeTag[TrivialSigmaIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TrivialSigmaCompanionCtor extends CompanionDef[TrivialSigmaCompanionCtor] with TrivialSigmaCompanion {
    def selfType = TrivialSigmaCompanionElem
    override def toString = "TrivialSigmaCompanion"

    @scalan.OverloadId("fromFields")
    def apply(isValid: Rep[Boolean]): Rep[TrivialSigma] =
      mkTrivialSigma(isValid)

    def unapply(p: Rep[Sigma]) = unmkTrivialSigma(p)
  }
  lazy val TrivialSigmaRep: Rep[TrivialSigmaCompanionCtor] = new TrivialSigmaCompanionCtor
  lazy val TrivialSigma: TrivialSigmaCompanionCtor = proxyTrivialSigmaCompanion(TrivialSigmaRep)
  implicit def proxyTrivialSigmaCompanion(p: Rep[TrivialSigmaCompanionCtor]): TrivialSigmaCompanionCtor = {
    proxyOps[TrivialSigmaCompanionCtor](p)
  }

  implicit case object TrivialSigmaCompanionElem extends CompanionElem[TrivialSigmaCompanionCtor] {
    lazy val tag = weakTypeTag[TrivialSigmaCompanionCtor]
    protected def getDefaultRep = TrivialSigmaRep
  }

  implicit def proxyTrivialSigma(p: Rep[TrivialSigma]): TrivialSigma =
    proxyOps[TrivialSigma](p)

  implicit class ExtendedTrivialSigma(p: Rep[TrivialSigma]) {
    def toData: Rep[TrivialSigmaData] = {
      isoTrivialSigma.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTrivialSigma: Iso[TrivialSigmaData, TrivialSigma] =
    reifyObject(new TrivialSigmaIso())

  case class ProveDlogEvidenceCtor
      (override val id: Rep[Int], override val isValid: Rep[Boolean])
    extends ProveDlogEvidence(id, isValid) with Def[ProveDlogEvidence] {
    lazy val selfType = element[ProveDlogEvidence]
  }
  // elem for concrete class
  class ProveDlogEvidenceElem(val iso: Iso[ProveDlogEvidenceData, ProveDlogEvidence])
    extends ProveDlogElem[ProveDlogEvidence]
    with ConcreteElem[ProveDlogEvidenceData, ProveDlogEvidence] {
    override lazy val parent: Option[Elem[_]] = Some(proveDlogElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertProveDlog(x: Rep[ProveDlog]) = // Converter is not generated by meta
!!!("Cannot convert from ProveDlog to ProveDlogEvidence: missing fields List(id)")
    override def getDefaultRep = ProveDlogEvidence(0, false)
    override lazy val tag = {
      weakTypeTag[ProveDlogEvidence]
    }
  }

  // state representation type
  type ProveDlogEvidenceData = (Int, Boolean)

  // 3) Iso for concrete class
  class ProveDlogEvidenceIso
    extends EntityIso[ProveDlogEvidenceData, ProveDlogEvidence] with Def[ProveDlogEvidenceIso] {
    private lazy val _safeFrom = fun { p: Rep[ProveDlogEvidence] => (p.id, p.isValid) }
    override def from(p: Rep[ProveDlogEvidence]) =
      tryConvert[ProveDlogEvidence, (Int, Boolean)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Int, Boolean)]) = {
      val Pair(id, isValid) = p
      ProveDlogEvidence(id, isValid)
    }
    lazy val eFrom = pairElement(element[Int], element[Boolean])
    lazy val eTo = new ProveDlogEvidenceElem(self)
    lazy val selfType = new ProveDlogEvidenceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ProveDlogEvidenceIsoElem() extends Elem[ProveDlogEvidenceIso] {
    def getDefaultRep = reifyObject(new ProveDlogEvidenceIso())
    lazy val tag = {
      weakTypeTag[ProveDlogEvidenceIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ProveDlogEvidenceCompanionCtor extends CompanionDef[ProveDlogEvidenceCompanionCtor] with ProveDlogEvidenceCompanion {
    def selfType = ProveDlogEvidenceCompanionElem
    override def toString = "ProveDlogEvidenceCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ProveDlogEvidenceData]): Rep[ProveDlogEvidence] = {
      isoProveDlogEvidence.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(id: Rep[Int], isValid: Rep[Boolean]): Rep[ProveDlogEvidence] =
      mkProveDlogEvidence(id, isValid)

    def unapply(p: Rep[ProveDlog]) = unmkProveDlogEvidence(p)
  }
  lazy val ProveDlogEvidenceRep: Rep[ProveDlogEvidenceCompanionCtor] = new ProveDlogEvidenceCompanionCtor
  lazy val ProveDlogEvidence: ProveDlogEvidenceCompanionCtor = proxyProveDlogEvidenceCompanion(ProveDlogEvidenceRep)
  implicit def proxyProveDlogEvidenceCompanion(p: Rep[ProveDlogEvidenceCompanionCtor]): ProveDlogEvidenceCompanionCtor = {
    proxyOps[ProveDlogEvidenceCompanionCtor](p)
  }

  implicit case object ProveDlogEvidenceCompanionElem extends CompanionElem[ProveDlogEvidenceCompanionCtor] {
    lazy val tag = weakTypeTag[ProveDlogEvidenceCompanionCtor]
    protected def getDefaultRep = ProveDlogEvidenceRep
  }

  implicit def proxyProveDlogEvidence(p: Rep[ProveDlogEvidence]): ProveDlogEvidence =
    proxyOps[ProveDlogEvidence](p)

  implicit class ExtendedProveDlogEvidence(p: Rep[ProveDlogEvidence]) {
    def toData: Rep[ProveDlogEvidenceData] = {
      isoProveDlogEvidence.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoProveDlogEvidence: Iso[ProveDlogEvidenceData, ProveDlogEvidence] =
    reifyObject(new ProveDlogEvidenceIso())

  case class CrowdFundingContractCtor
      (override val timeout: Rep[Int], override val minToRaise: Rep[Int], override val backerPubKey: Rep[ProveDlog], override val projectPubKey: Rep[ProveDlog])
    extends CrowdFundingContract(timeout, minToRaise, backerPubKey, projectPubKey) with Def[CrowdFundingContract] {
    lazy val selfType = element[CrowdFundingContract]
  }
  // elem for concrete class
  class CrowdFundingContractElem(val iso: Iso[CrowdFundingContractData, CrowdFundingContract])
    extends CrowdFundingElem[CrowdFundingContract]
    with ConcreteElem[CrowdFundingContractData, CrowdFundingContract] {
    override lazy val parent: Option[Elem[_]] = Some(crowdFundingElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCrowdFunding(x: Rep[CrowdFunding]) = CrowdFundingContract(x.timeout, x.minToRaise, x.backerPubKey, x.projectPubKey)
    override def getDefaultRep = CrowdFundingContract(0, 0, element[ProveDlog].defaultRepValue, element[ProveDlog].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[CrowdFundingContract]
    }
  }

  // state representation type
  type CrowdFundingContractData = (Int, (Int, (ProveDlog, ProveDlog)))

  // 3) Iso for concrete class
  class CrowdFundingContractIso
    extends EntityIso[CrowdFundingContractData, CrowdFundingContract] with Def[CrowdFundingContractIso] {
    private lazy val _safeFrom = fun { p: Rep[CrowdFundingContract] => (p.timeout, p.minToRaise, p.backerPubKey, p.projectPubKey) }
    override def from(p: Rep[CrowdFundingContract]) =
      tryConvert[CrowdFundingContract, (Int, (Int, (ProveDlog, ProveDlog)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Int, (Int, (ProveDlog, ProveDlog)))]) = {
      val Pair(timeout, Pair(minToRaise, Pair(backerPubKey, projectPubKey))) = p
      CrowdFundingContract(timeout, minToRaise, backerPubKey, projectPubKey)
    }
    lazy val eFrom = pairElement(element[Int], pairElement(element[Int], pairElement(element[ProveDlog], element[ProveDlog])))
    lazy val eTo = new CrowdFundingContractElem(self)
    lazy val selfType = new CrowdFundingContractIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CrowdFundingContractIsoElem() extends Elem[CrowdFundingContractIso] {
    def getDefaultRep = reifyObject(new CrowdFundingContractIso())
    lazy val tag = {
      weakTypeTag[CrowdFundingContractIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CrowdFundingContractCompanionCtor extends CompanionDef[CrowdFundingContractCompanionCtor] with CrowdFundingContractCompanion {
    def selfType = CrowdFundingContractCompanionElem
    override def toString = "CrowdFundingContractCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CrowdFundingContractData]): Rep[CrowdFundingContract] = {
      isoCrowdFundingContract.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(timeout: Rep[Int], minToRaise: Rep[Int], backerPubKey: Rep[ProveDlog], projectPubKey: Rep[ProveDlog]): Rep[CrowdFundingContract] =
      mkCrowdFundingContract(timeout, minToRaise, backerPubKey, projectPubKey)

    def unapply(p: Rep[CrowdFunding]) = unmkCrowdFundingContract(p)
  }
  lazy val CrowdFundingContractRep: Rep[CrowdFundingContractCompanionCtor] = new CrowdFundingContractCompanionCtor
  lazy val CrowdFundingContract: CrowdFundingContractCompanionCtor = proxyCrowdFundingContractCompanion(CrowdFundingContractRep)
  implicit def proxyCrowdFundingContractCompanion(p: Rep[CrowdFundingContractCompanionCtor]): CrowdFundingContractCompanionCtor = {
    proxyOps[CrowdFundingContractCompanionCtor](p)
  }

  implicit case object CrowdFundingContractCompanionElem extends CompanionElem[CrowdFundingContractCompanionCtor] {
    lazy val tag = weakTypeTag[CrowdFundingContractCompanionCtor]
    protected def getDefaultRep = CrowdFundingContractRep
  }

  implicit def proxyCrowdFundingContract(p: Rep[CrowdFundingContract]): CrowdFundingContract =
    proxyOps[CrowdFundingContract](p)

  implicit class ExtendedCrowdFundingContract(p: Rep[CrowdFundingContract]) {
    def toData: Rep[CrowdFundingContractData] = {
      isoCrowdFundingContract.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCrowdFundingContract: Iso[CrowdFundingContractData, CrowdFundingContract] =
    reifyObject(new CrowdFundingContractIso())

  registerModule(SigmaDslOverArraysModule)

  object ContextOverArraysMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[ContextOverArrays]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextOverArraysElem] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[ContextOverArrays]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ContextOverArrays]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Option[Rep[ContextOverArrays]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextOverArraysElem] && method.getName == "INPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[ContextOverArrays]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ContextOverArrays]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Option[Rep[ContextOverArrays]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextOverArraysElem] && method.getName == "OUTPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[ContextOverArrays]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ContextOverArrays]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ContextOverArraysCompanionMethods {
  }

  def mkContextOverArrays
    (inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], HEIGHT: Rep[Long]): Rep[ContextOverArrays] = {
    new ContextOverArraysCtor(inputs, outputs, HEIGHT)
  }
  def unmkContextOverArrays(p: Rep[Context]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ContextOverArraysElem @unchecked =>
      Some((p.asRep[ContextOverArrays].inputs, p.asRep[ContextOverArrays].outputs, p.asRep[ContextOverArrays].HEIGHT))
    case _ =>
      None
  }

  object ContextOverArrayBuilderMethods {
    object Collections {
      def unapply(d: Def[_]): Option[Rep[ContextOverArrayBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextOverArrayBuilderElem] && method.getName == "Collections" =>
          Some(receiver).asInstanceOf[Option[Rep[ContextOverArrayBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ContextOverArrayBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ContextOverArrayBuilderCompanionMethods {
  }

  def mkContextOverArrayBuilder
    (): Rep[ContextOverArrayBuilder] = {
    new ContextOverArrayBuilderCtor()
  }
  def unmkContextOverArrayBuilder(p: Rep[ContextBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ContextOverArrayBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

  object TrivialSigmaMethods {
  }

  object TrivialSigmaCompanionMethods {
  }

  def mkTrivialSigma
    (isValid: Rep[Boolean]): Rep[TrivialSigma] = {
    new TrivialSigmaCtor(isValid)
  }
  def unmkTrivialSigma(p: Rep[Sigma]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TrivialSigmaElem @unchecked =>
      Some((p.asRep[TrivialSigma].isValid))
    case _ =>
      None
  }

  object ProveDlogEvidenceMethods {
    object propBytes {
      def unapply(d: Def[_]): Option[Rep[ProveDlogEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[ProveDlogEvidence]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ProveDlogEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ProveDlogEvidenceCompanionMethods {
  }

  def mkProveDlogEvidence
    (id: Rep[Int], isValid: Rep[Boolean]): Rep[ProveDlogEvidence] = {
    new ProveDlogEvidenceCtor(id, isValid)
  }
  def unmkProveDlogEvidence(p: Rep[ProveDlog]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ProveDlogEvidenceElem @unchecked =>
      Some((p.asRep[ProveDlogEvidence].id, p.asRep[ProveDlogEvidence].isValid))
    case _ =>
      None
  }

  object CrowdFundingContractMethods {
  }

  object CrowdFundingContractCompanionMethods {
  }

  def mkCrowdFundingContract
    (timeout: Rep[Int], minToRaise: Rep[Int], backerPubKey: Rep[ProveDlog], projectPubKey: Rep[ProveDlog]): Rep[CrowdFundingContract] = {
    new CrowdFundingContractCtor(timeout, minToRaise, backerPubKey, projectPubKey)
  }
  def unmkCrowdFundingContract(p: Rep[CrowdFunding]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CrowdFundingContractElem @unchecked =>
      Some((p.asRep[CrowdFundingContract].timeout, p.asRep[CrowdFundingContract].minToRaise, p.asRep[CrowdFundingContract].backerPubKey, p.asRep[CrowdFundingContract].projectPubKey))
    case _ =>
      None
  }
}

object SigmaDslOverArraysModule extends scalan.ModuleInfo("special.sigma", "SigmaDslOverArrays")
}

trait SigmaDslOverArraysModule extends special.sigma.impl.SigmaDslOverArraysDefs {self: SigmaLibrary =>}
