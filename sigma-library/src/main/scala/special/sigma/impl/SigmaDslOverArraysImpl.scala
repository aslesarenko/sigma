package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslOverArraysDefs extends scalan.Scalan with SigmaDslOverArrays {
  self: SigmaLibrary =>
  import Context._
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
    object builder {
      def unapply(d: Def[_]): Option[Rep[DefaultSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[DefaultSigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DefaultSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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
    object builder {
      def unapply(d: Def[_]): Option[Rep[DefaultContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[DefaultContract]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[DefaultContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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

  case class TestBoxCtor
      (override val idBytes: Rep[WArray[Byte]], override val value: Rep[Long], override val propositionBytes: Rep[Col[Byte]], override val registers: Rep[Col[AnyValue]])
    extends TestBox(idBytes, value, propositionBytes, registers) with Def[TestBox] {
    lazy val selfType = element[TestBox]
  }
  // elem for concrete class
  class TestBoxElem(val iso: Iso[TestBoxData, TestBox])
    extends BoxElem[TestBox]
    with ConcreteElem[TestBoxData, TestBox] {
    override lazy val parent: Option[Elem[_]] = Some(boxElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertBox(x: Rep[Box]) = // Converter is not generated by meta
!!!("Cannot convert from Box to TestBox: missing fields List(idBytes)")
    override def getDefaultRep = TestBox(element[WArray[Byte]].defaultRepValue, 0l, element[Col[Byte]].defaultRepValue, element[Col[AnyValue]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestBox]
    }
  }

  // state representation type
  type TestBoxData = (WArray[Byte], (Long, (Col[Byte], Col[AnyValue])))

  // 3) Iso for concrete class
  class TestBoxIso
    extends EntityIso[TestBoxData, TestBox] with Def[TestBoxIso] {
    private lazy val _safeFrom = fun { p: Rep[TestBox] => (p.idBytes, p.value, p.propositionBytes, p.registers) }
    override def from(p: Rep[TestBox]) =
      tryConvert[TestBox, (WArray[Byte], (Long, (Col[Byte], Col[AnyValue])))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WArray[Byte], (Long, (Col[Byte], Col[AnyValue])))]) = {
      val Pair(idBytes, Pair(value, Pair(propositionBytes, registers))) = p
      TestBox(idBytes, value, propositionBytes, registers)
    }
    lazy val eFrom = pairElement(element[WArray[Byte]], pairElement(element[Long], pairElement(element[Col[Byte]], element[Col[AnyValue]])))
    lazy val eTo = new TestBoxElem(self)
    lazy val selfType = new TestBoxIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestBoxIsoElem() extends Elem[TestBoxIso] {
    def getDefaultRep = reifyObject(new TestBoxIso())
    lazy val tag = {
      weakTypeTag[TestBoxIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestBoxCompanionCtor extends CompanionDef[TestBoxCompanionCtor] with TestBoxCompanion {
    def selfType = TestBoxCompanionElem
    override def toString = "TestBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestBoxData]): Rep[TestBox] = {
      isoTestBox.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(idBytes: Rep[WArray[Byte]], value: Rep[Long], propositionBytes: Rep[Col[Byte]], registers: Rep[Col[AnyValue]]): Rep[TestBox] =
      mkTestBox(idBytes, value, propositionBytes, registers)

    def unapply(p: Rep[Box]) = unmkTestBox(p)
  }
  lazy val TestBoxRep: Rep[TestBoxCompanionCtor] = new TestBoxCompanionCtor
  lazy val TestBox: TestBoxCompanionCtor = proxyTestBoxCompanion(TestBoxRep)
  implicit def proxyTestBoxCompanion(p: Rep[TestBoxCompanionCtor]): TestBoxCompanionCtor = {
    proxyOps[TestBoxCompanionCtor](p)
  }

  implicit case object TestBoxCompanionElem extends CompanionElem[TestBoxCompanionCtor] {
    lazy val tag = weakTypeTag[TestBoxCompanionCtor]
    protected def getDefaultRep = TestBoxRep
  }

  implicit def proxyTestBox(p: Rep[TestBox]): TestBox =
    proxyOps[TestBox](p)

  implicit class ExtendedTestBox(p: Rep[TestBox]) {
    def toData: Rep[TestBoxData] = {
      isoTestBox.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestBox: Iso[TestBoxData, TestBox] =
    reifyObject(new TestBoxIso())

  case class TestValueCtor[T]
      (override val value: Rep[T])
    extends TestValue[T](value) with Def[TestValue[T]] {
    implicit lazy val eT = value.elem

    lazy val selfType = element[TestValue[T]]
  }
  // elem for concrete class
  class TestValueElem[T](val iso: Iso[TestValueData[T], TestValue[T]])(implicit val eT: Elem[T])
    extends AnyValueElem[TestValue[T]]
    with ConcreteElem[TestValueData[T], TestValue[T]] {
    override lazy val parent: Option[Elem[_]] = Some(anyValueElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override def convertAnyValue(x: Rep[AnyValue]) = // Converter is not generated by meta
!!!("Cannot convert from AnyValue to TestValue: missing fields List(value)")
    override def getDefaultRep = TestValue(element[T].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[TestValue[T]]
    }
  }

  // state representation type
  type TestValueData[T] = T

  // 3) Iso for concrete class
  class TestValueIso[T](implicit eT: Elem[T])
    extends EntityIso[TestValueData[T], TestValue[T]] with Def[TestValueIso[T]] {
    private lazy val _safeFrom = fun { p: Rep[TestValue[T]] => p.value }
    override def from(p: Rep[TestValue[T]]) =
      tryConvert[TestValue[T], T](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[T]) = {
      val value = p
      TestValue(value)
    }
    lazy val eFrom = element[T]
    lazy val eTo = new TestValueElem[T](self)
    lazy val selfType = new TestValueIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class TestValueIsoElem[T](eT: Elem[T]) extends Elem[TestValueIso[T]] {
    def getDefaultRep = reifyObject(new TestValueIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[TestValueIso[T]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class TestValueCompanionCtor extends CompanionDef[TestValueCompanionCtor] with TestValueCompanion {
    def selfType = TestValueCompanionElem
    override def toString = "TestValueCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T](value: Rep[T]): Rep[TestValue[T]] =
      mkTestValue(value)

    def unapply[T](p: Rep[AnyValue]) = unmkTestValue(p)
  }
  lazy val TestValueRep: Rep[TestValueCompanionCtor] = new TestValueCompanionCtor
  lazy val TestValue: TestValueCompanionCtor = proxyTestValueCompanion(TestValueRep)
  implicit def proxyTestValueCompanion(p: Rep[TestValueCompanionCtor]): TestValueCompanionCtor = {
    proxyOps[TestValueCompanionCtor](p)
  }

  implicit case object TestValueCompanionElem extends CompanionElem[TestValueCompanionCtor] {
    lazy val tag = weakTypeTag[TestValueCompanionCtor]
    protected def getDefaultRep = TestValueRep
  }

  implicit def proxyTestValue[T](p: Rep[TestValue[T]]): TestValue[T] =
    proxyOps[TestValue[T]](p)

  implicit class ExtendedTestValue[T](p: Rep[TestValue[T]]) {
    def toData: Rep[TestValueData[T]] = {
      implicit val eT = p.value.elem
      isoTestValue(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestValue[T](implicit eT: Elem[T]): Iso[TestValueData[T], TestValue[T]] =
    reifyObject(new TestValueIso[T]()(eT))

  case class TestContextCtor
      (override val inputs: Rep[WArray[Box]], override val outputs: Rep[WArray[Box]], override val height: Rep[Long], override val selfBox: Rep[Box], override val vars: Rep[WArray[AnyValue]])
    extends TestContext(inputs, outputs, height, selfBox, vars) with Def[TestContext] {
    lazy val selfType = element[TestContext]
  }
  // elem for concrete class
  class TestContextElem(val iso: Iso[TestContextData, TestContext])
    extends ContextElem[TestContext]
    with ConcreteElem[TestContextData, TestContext] {
    override lazy val parent: Option[Elem[_]] = Some(contextElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertContext(x: Rep[Context]) = // Converter is not generated by meta
!!!("Cannot convert from Context to TestContext: missing fields List(inputs, outputs, height, selfBox, vars)")
    override def getDefaultRep = TestContext(element[WArray[Box]].defaultRepValue, element[WArray[Box]].defaultRepValue, 0l, element[Box].defaultRepValue, element[WArray[AnyValue]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestContext]
    }
  }

  // state representation type
  type TestContextData = (WArray[Box], (WArray[Box], (Long, (Box, WArray[AnyValue]))))

  // 3) Iso for concrete class
  class TestContextIso
    extends EntityIso[TestContextData, TestContext] with Def[TestContextIso] {
    private lazy val _safeFrom = fun { p: Rep[TestContext] => (p.inputs, p.outputs, p.height, p.selfBox, p.vars) }
    override def from(p: Rep[TestContext]) =
      tryConvert[TestContext, (WArray[Box], (WArray[Box], (Long, (Box, WArray[AnyValue]))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WArray[Box], (WArray[Box], (Long, (Box, WArray[AnyValue]))))]) = {
      val Pair(inputs, Pair(outputs, Pair(height, Pair(selfBox, vars)))) = p
      TestContext(inputs, outputs, height, selfBox, vars)
    }
    lazy val eFrom = pairElement(element[WArray[Box]], pairElement(element[WArray[Box]], pairElement(element[Long], pairElement(element[Box], element[WArray[AnyValue]]))))
    lazy val eTo = new TestContextElem(self)
    lazy val selfType = new TestContextIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestContextIsoElem() extends Elem[TestContextIso] {
    def getDefaultRep = reifyObject(new TestContextIso())
    lazy val tag = {
      weakTypeTag[TestContextIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestContextCompanionCtor extends CompanionDef[TestContextCompanionCtor] with TestContextCompanion {
    def selfType = TestContextCompanionElem
    override def toString = "TestContextCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestContextData]): Rep[TestContext] = {
      isoTestContext.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], height: Rep[Long], selfBox: Rep[Box], vars: Rep[WArray[AnyValue]]): Rep[TestContext] =
      mkTestContext(inputs, outputs, height, selfBox, vars)

    def unapply(p: Rep[Context]) = unmkTestContext(p)
  }
  lazy val TestContextRep: Rep[TestContextCompanionCtor] = new TestContextCompanionCtor
  lazy val TestContext: TestContextCompanionCtor = proxyTestContextCompanion(TestContextRep)
  implicit def proxyTestContextCompanion(p: Rep[TestContextCompanionCtor]): TestContextCompanionCtor = {
    proxyOps[TestContextCompanionCtor](p)
  }

  implicit case object TestContextCompanionElem extends CompanionElem[TestContextCompanionCtor] {
    lazy val tag = weakTypeTag[TestContextCompanionCtor]
    protected def getDefaultRep = TestContextRep
  }

  implicit def proxyTestContext(p: Rep[TestContext]): TestContext =
    proxyOps[TestContext](p)

  implicit class ExtendedTestContext(p: Rep[TestContext]) {
    def toData: Rep[TestContextData] = {
      isoTestContext.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestContext: Iso[TestContextData, TestContext] =
    reifyObject(new TestContextIso())

  case class TestSigmaDslBuilderCtor
      ()
    extends TestSigmaDslBuilder() with Def[TestSigmaDslBuilder] {
    lazy val selfType = element[TestSigmaDslBuilder]
  }
  // elem for concrete class
  class TestSigmaDslBuilderElem(val iso: Iso[TestSigmaDslBuilderData, TestSigmaDslBuilder])
    extends SigmaDslBuilderElem[TestSigmaDslBuilder]
    with ConcreteElem[TestSigmaDslBuilderData, TestSigmaDslBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaDslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSigmaDslBuilder(x: Rep[SigmaDslBuilder]) = TestSigmaDslBuilder()
    override def getDefaultRep = TestSigmaDslBuilder()
    override lazy val tag = {
      weakTypeTag[TestSigmaDslBuilder]
    }
  }

  // state representation type
  type TestSigmaDslBuilderData = Unit

  // 3) Iso for concrete class
  class TestSigmaDslBuilderIso
    extends EntityIso[TestSigmaDslBuilderData, TestSigmaDslBuilder] with Def[TestSigmaDslBuilderIso] {
    private lazy val _safeFrom = fun { p: Rep[TestSigmaDslBuilder] => () }
    override def from(p: Rep[TestSigmaDslBuilder]) =
      tryConvert[TestSigmaDslBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      TestSigmaDslBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new TestSigmaDslBuilderElem(self)
    lazy val selfType = new TestSigmaDslBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestSigmaDslBuilderIsoElem() extends Elem[TestSigmaDslBuilderIso] {
    def getDefaultRep = reifyObject(new TestSigmaDslBuilderIso())
    lazy val tag = {
      weakTypeTag[TestSigmaDslBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestSigmaDslBuilderCompanionCtor extends CompanionDef[TestSigmaDslBuilderCompanionCtor] with TestSigmaDslBuilderCompanion {
    def selfType = TestSigmaDslBuilderCompanionElem
    override def toString = "TestSigmaDslBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestSigmaDslBuilderData]): Rep[TestSigmaDslBuilder] = {
      isoTestSigmaDslBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[TestSigmaDslBuilder] =
      mkTestSigmaDslBuilder()

    def unapply(p: Rep[SigmaDslBuilder]) = unmkTestSigmaDslBuilder(p)
  }
  lazy val TestSigmaDslBuilderRep: Rep[TestSigmaDslBuilderCompanionCtor] = new TestSigmaDslBuilderCompanionCtor
  lazy val TestSigmaDslBuilder: TestSigmaDslBuilderCompanionCtor = proxyTestSigmaDslBuilderCompanion(TestSigmaDslBuilderRep)
  implicit def proxyTestSigmaDslBuilderCompanion(p: Rep[TestSigmaDslBuilderCompanionCtor]): TestSigmaDslBuilderCompanionCtor = {
    proxyOps[TestSigmaDslBuilderCompanionCtor](p)
  }

  implicit case object TestSigmaDslBuilderCompanionElem extends CompanionElem[TestSigmaDslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[TestSigmaDslBuilderCompanionCtor]
    protected def getDefaultRep = TestSigmaDslBuilderRep
  }

  implicit def proxyTestSigmaDslBuilder(p: Rep[TestSigmaDslBuilder]): TestSigmaDslBuilder =
    proxyOps[TestSigmaDslBuilder](p)

  implicit class ExtendedTestSigmaDslBuilder(p: Rep[TestSigmaDslBuilder]) {
    def toData: Rep[TestSigmaDslBuilderData] = {
      isoTestSigmaDslBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestSigmaDslBuilder: Iso[TestSigmaDslBuilderData, TestSigmaDslBuilder] =
    reifyObject(new TestSigmaDslBuilderIso())

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
      (override val value: Rep[WECPoint])
    extends ProveDlogEvidence(value) with Def[ProveDlogEvidence] {
    lazy val selfType = element[ProveDlogEvidence]
  }
  // elem for concrete class
  class ProveDlogEvidenceElem(val iso: Iso[ProveDlogEvidenceData, ProveDlogEvidence])
    extends ProveDlogElem[ProveDlogEvidence]
    with ConcreteElem[ProveDlogEvidenceData, ProveDlogEvidence] {
    override lazy val parent: Option[Elem[_]] = Some(proveDlogElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertProveDlog(x: Rep[ProveDlog]) = ProveDlogEvidence(x.value)
    override def getDefaultRep = ProveDlogEvidence(element[WECPoint].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[ProveDlogEvidence]
    }
  }

  // state representation type
  type ProveDlogEvidenceData = WECPoint

  // 3) Iso for concrete class
  class ProveDlogEvidenceIso
    extends EntityIso[ProveDlogEvidenceData, ProveDlogEvidence] with Def[ProveDlogEvidenceIso] {
    private lazy val _safeFrom = fun { p: Rep[ProveDlogEvidence] => p.value }
    override def from(p: Rep[ProveDlogEvidence]) =
      tryConvert[ProveDlogEvidence, WECPoint](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WECPoint]) = {
      val value = p
      ProveDlogEvidence(value)
    }
    lazy val eFrom = element[WECPoint]
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

    @scalan.OverloadId("fromFields")
    def apply(value: Rep[WECPoint]): Rep[ProveDlogEvidence] =
      mkProveDlogEvidence(value)

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

  registerModule(SigmaDslOverArraysModule)

  object TestBoxMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[TestBox]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object id {
      def unapply(d: Def[_]): Option[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "id" =>
          Some(receiver).asInstanceOf[Option[Rep[TestBox]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Option[(Rep[TestBox], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, emT, _*), _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "getReg" =>
          Some((receiver, i, emT)).asInstanceOf[Option[(Rep[TestBox], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestBox], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[TestBox]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestBoxCompanionMethods {
  }

  def mkTestBox
    (idBytes: Rep[WArray[Byte]], value: Rep[Long], propositionBytes: Rep[Col[Byte]], registers: Rep[Col[AnyValue]]): Rep[TestBox] = {
    new TestBoxCtor(idBytes, value, propositionBytes, registers)
  }
  def unmkTestBox(p: Rep[Box]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestBoxElem @unchecked =>
      Some((p.asRep[TestBox].idBytes, p.asRep[TestBox].value, p.asRep[TestBox].propositionBytes, p.asRep[TestBox].registers))
    case _ =>
      None
  }

  object TestValueMethods {
    object cost {
      def unapply(d: Def[_]): Option[Rep[TestValue[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestValueElem[_]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[TestValue[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestValue[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestValueCompanionMethods {
  }

  def mkTestValue[T]
    (value: Rep[T]): Rep[TestValue[T]] = {
    new TestValueCtor[T](value)
  }
  def unmkTestValue[T](p: Rep[AnyValue]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestValueElem[T] @unchecked =>
      Some((p.asRep[TestValue[T]].value))
    case _ =>
      None
  }

  object TestContextMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "HEIGHT" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "SELF" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "INPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Option[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "OUTPUTS" =>
          Some(receiver).asInstanceOf[Option[Rep[TestContext]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Option[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(id, emT, _*), _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "getVar" =>
          Some((receiver, id, emT)).asInstanceOf[Option[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestContextCompanionMethods {
  }

  def mkTestContext
    (inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], height: Rep[Long], selfBox: Rep[Box], vars: Rep[WArray[AnyValue]]): Rep[TestContext] = {
    new TestContextCtor(inputs, outputs, height, selfBox, vars)
  }
  def unmkTestContext(p: Rep[Context]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestContextElem @unchecked =>
      Some((p.asRep[TestContext].inputs, p.asRep[TestContext].outputs, p.asRep[TestContext].height, p.asRep[TestContext].selfBox, p.asRep[TestContext].vars))
    case _ =>
      None
  }

  object TestSigmaDslBuilderMethods {
    object Cols {
      def unapply(d: Def[_]): Option[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "Cols" =>
          Some(receiver).asInstanceOf[Option[Rep[TestSigmaDslBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object TestSigmaDslBuilderCompanionMethods {
  }

  def mkTestSigmaDslBuilder
    (): Rep[TestSigmaDslBuilder] = {
    new TestSigmaDslBuilderCtor()
  }
  def unmkTestSigmaDslBuilder(p: Rep[SigmaDslBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestSigmaDslBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

  object TrivialSigmaMethods {
    object propBytes {
      def unapply(d: Def[_]): Option[Rep[TrivialSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "propBytes" =>
          Some(receiver).asInstanceOf[Option[Rep[TrivialSigma]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[TrivialSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
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

    object isValid {
      def unapply(d: Def[_]): Option[Rep[ProveDlogEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "isValid" =>
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
    (value: Rep[WECPoint]): Rep[ProveDlogEvidence] = {
    new ProveDlogEvidenceCtor(value)
  }
  def unmkProveDlogEvidence(p: Rep[ProveDlog]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ProveDlogEvidenceElem @unchecked =>
      Some((p.asRep[ProveDlogEvidence].value))
    case _ =>
      None
  }
}

object SigmaDslOverArraysModule extends scalan.ModuleInfo("special.sigma", "SigmaDslOverArrays")
}

trait SigmaDslOverArraysModule extends special.sigma.impl.SigmaDslOverArraysDefs {self: SigmaLibrary =>}
