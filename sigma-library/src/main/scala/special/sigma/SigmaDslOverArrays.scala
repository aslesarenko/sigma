package special.sigma {
  import scalan.OverloadHack.Overloaded1  // manual fix
  import scalan._

  trait SigmaDslOverArrays extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import CostedBuilder._  // manual fix
    import CCostedBuilder._;
    import Col._;
    import ColBuilder._ // manual fix
    import ColOverArrayBuilder._;
    import Context._;
    import CostModel._;
    import CostedCol._;
    import CostedOption._;
    import DefaultSigma._;
    import MonoidBuilderInst._;
    import SigmaContract._;
    import SigmaDslBuilder._;
    import SigmaProp._;
    import TestSigmaDslBuilder._;
    import WBigInteger._;
    import WECPoint._;
    import WOption._;
    import CostedNone._; // manual fix
    import CostedSome._; // manuaf fix
    import WSpecialPredef._; // manuaf fix
    trait DefaultSigma extends SigmaProp {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      @NeverInline @OverloadId(value = "and_sigma") def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix
      @NeverInline @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline @OverloadId(value = "or_sigma") def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix
      @NeverInline @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    };
    trait DefaultContract extends SigmaContract {
      def builder: Rep[SigmaDslBuilder] = RTestSigmaDslBuilder()
    };
    abstract class TestBox(val id: Rep[Col[Byte]], val value: Rep[Long], val bytes: Rep[Col[Byte]], val bytesWithoutRef: Rep[Col[Byte]], val propositionBytes: Rep[Col[Byte]], val registers: Rep[Col[AnyValue]]) extends Box {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      @NeverInline def getReg[T](id: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = delayInvoke;
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke;
      def creationInfo: Rep[scala.Tuple2[Long, Col[Byte]]] = this.R3[scala.Tuple2[Long, Col[Byte]]].get;
      def tokens: Rep[Col[scala.Tuple2[Col[Byte], Long]]] = this.R2[Col[scala.Tuple2[Col[Byte], Long]]].get
    };
    abstract class TestAvlTree(val startingDigest: Rep[Col[Byte]], val keyLength: Rep[Int], val valueLengthOpt: Rep[WOption[Int]], val maxNumOperations: Rep[WOption[Int]], val maxDeletes: Rep[WOption[Int]]) extends AvlTree with Product with Serializable {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      @NeverInline def dataSize: Rep[Long] = delayInvoke;
      @NeverInline def cost: Rep[Int] = delayInvoke
    };
    abstract class TestValue[T](val value: Rep[T]) extends AnyValue {
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class TestContext(val inputs: Rep[WArray[Box]], val outputs: Rep[WArray[Box]], val height: Rep[Long], val selfBox: Rep[Box], val lastBlockUtxoRootHash: Rep[AvlTree], val minerPubKey: Rep[WArray[Byte]], val vars: Rep[WArray[AnyValue]]) extends Context {
      def builder: Rep[TestSigmaDslBuilder] = RTestSigmaDslBuilder();
      @NeverInline def HEIGHT: Rep[Long] = delayInvoke;
      @NeverInline def SELF: Rep[Box] = delayInvoke;
      @NeverInline def INPUTS: Rep[Col[Box]] = delayInvoke;
      @NeverInline def OUTPUTS: Rep[Col[Box]] = delayInvoke;
      @NeverInline def LastBlockUtxoRootHash: Rep[AvlTree] = delayInvoke;
      @NeverInline def MinerPubKey: Rep[Col[Byte]] = delayInvoke;
      @NeverInline def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = delayInvoke;
      @NeverInline def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = delayInvoke ;
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class TestSigmaDslBuilder extends SigmaDslBuilder {
      // manual fix
      def Cols: Rep[ColBuilder] = RColOverArrayBuilder();
      def Monoids: Rep[MonoidBuilderInst] = RMonoidBuilderInst();
      // manual fix
      def Costing: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def CostModel: Rep[CostModel] = delayInvoke;
      def costBoxes(bs: Rep[Col[Box]]): Rep[CostedCol[Box]] = {
        val len: Rep[Int] = bs.length;
        val perItemCost: Rep[Int] = this.CostModel.AccessBox;
        val costs: Rep[Col[Int]] = this.Cols.replicate[Int](len, perItemCost);
        val sizes: Rep[Col[Long]] = bs.map[Long](fun(((b: Rep[Box]) => b.dataSize)));
        val valuesCost: Rep[Int] = this.CostModel.CollectionConst;
        this.Costing.mkCostedCol[Box](bs, costs, sizes, valuesCost)
      };
      def costColWithConstSizedItem[T](xs: Rep[Col[T]], len: Rep[Int], itemSize: Rep[Long]): Rep[CostedCol[T]] = {
        // manual fix (div)
        val perItemCost: Rep[Long] = len.toLong.*(itemSize).div(toRep(1024L.asInstanceOf[Long])).+(toRep(1.asInstanceOf[Long])).*(this.CostModel.AccessKiloByteOfData.toLong);
        val costs: Rep[Col[Int]] = this.Cols.replicate[Int](len, perItemCost.toInt);
        val sizes: Rep[Col[Long]] = this.Cols.replicate[Long](len, itemSize);
        val valueCost: Rep[Int] = this.CostModel.CollectionConst;
        this.Costing.mkCostedCol[T](xs, costs, sizes, valueCost)
      };
      def costOption[T](opt: Rep[WOption[T]], opCost: Rep[Int]): Rep[CostedOption[T]] = {
        implicit val eT = opt.elem.eItem
        val none = Thunk(RCostedNone[T](opCost));
        opt.fold[CostedOption[T]](none,
          fun(((x: Rep[T]) => this.Costing.mkCostedSome[T](this.Costing.costedValue[T](x, RWSpecialPredef.some[Int](opCost))))))
      };
      @NeverInline def verifyZK(proof: Rep[Thunk[SigmaProp]]): Rep[Boolean] = delayInvoke;
      @NeverInline def atLeast(bound: Rep[Int], props: Rep[Col[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = delayInvoke;
      @NeverInline def allZK(proofs: Rep[Col[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def anyZK(proofs: Rep[Col[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = delayInvoke;
      @NeverInline def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = delayInvoke;
      @NeverInline def PubKey(base64String: Rep[String]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger] = delayInvoke;
      @NeverInline def longToByteArray(l: Rep[Long]): Rep[Col[Byte]] = delayInvoke;
      @NeverInline def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp] = delayInvoke;
      @NeverInline def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean] = delayInvoke;
      @NeverInline def treeLookup(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = delayInvoke;
      @NeverInline def treeModifications(tree: Rep[AvlTree], operations: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = delayInvoke;
      @NeverInline def groupGenerator: Rep[WECPoint] = delayInvoke;
      @NeverInline def exponentiate(base: Rep[WECPoint], exponent: Rep[WBigInteger]): Rep[WECPoint] = delayInvoke
    };
    abstract class TrivialSigma(val _isValid: Rep[Boolean]) extends SigmaProp with DefaultSigma with Product with Serializable {
      @NeverInline def propBytes: Rep[Col[Byte]] = delayInvoke;
      @NeverInline def isValid: Rep[Boolean] = delayInvoke;
      @NeverInline @OverloadId(value = "and_sigma") override def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix (implicit Overloaded)
      @NeverInline @OverloadId(value = "and_bool") override def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline @OverloadId(value = "or_sigma") override def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix (implicit Overloaded)
      @NeverInline @OverloadId(value = "or_bool") override def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    };
    abstract class ProveDlogEvidence(val value: Rep[WECPoint]) extends SigmaProp with DefaultSigma with Product with Serializable {
      @NeverInline def propBytes: Rep[Col[Byte]] = delayInvoke;
      @NeverInline def isValid: Rep[Boolean] = delayInvoke;
      @NeverInline @OverloadId(value = "and_sigma") override def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix (implicit Overloaded)
      @NeverInline @OverloadId(value = "and_bool") override def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline @OverloadId(value = "or_sigma") override def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix (implicit Overloaded)
      @NeverInline @OverloadId(value = "or_bool") override def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    };
    abstract class ProveDHTEvidence(val gv: Rep[WECPoint], val hv: Rep[WECPoint], val uv: Rep[WECPoint], val vv: Rep[WECPoint]) extends SigmaProp with DefaultSigma with Product with Serializable {
      @NeverInline def propBytes: Rep[Col[Byte]] = delayInvoke;
      @NeverInline def isValid: Rep[Boolean] = delayInvoke;
      @NeverInline @OverloadId(value = "and_sigma") override def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix (implicit Overloaded)
      @NeverInline @OverloadId(value = "and_bool") override def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline @OverloadId(value = "or_sigma") override def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = delayInvoke;
      // manual fix (implicit Overloaded)
      @NeverInline @OverloadId(value = "or_bool") override def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke;
      @NeverInline override def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = delayInvoke
    };
    trait DefaultSigmaCompanion;
    trait DefaultContractCompanion;
    trait TestBoxCompanion;
    trait TestAvlTreeCompanion;
    trait TestValueCompanion;
    trait TestContextCompanion;
    trait TestSigmaDslBuilderCompanion;
    trait TrivialSigmaCompanion;
    trait ProveDlogEvidenceCompanion;
    trait ProveDHTEvidenceCompanion
  }
}