package special.sigma {
  import scalan.OverloadHack.Overloaded1
  import scalan._

  trait SigmaDsl extends Base { self: SigmaLibrary =>
    import DslBuilder._;
    import SigmaDslBuilder._;
    import DslObject._;
    import Col._;
    import Sigma._;
    import WECPoint._;
    import AnyValue._;
    import WOption._;
    import Box._;
    import AvlTree._;
    import Context._;
    import WBigInteger._;
    import SigmaContract._;
    import ColBuilder._;
    import SigmaBuilder._;
    import BoxBuilder._;
    import AvlTreeBuilder._;
    import ContextBuilder._;
    import SigmaContractBuilder._;
    trait DslBuilder extends Def[DslBuilder];
    trait DslObject extends Def[DslObject] {
      def builder: Rep[SigmaDslBuilder]
    };
    @sigmalang trait Sigma extends DslObject {
      def isValid: Rep[Boolean];
      def propBytes: Rep[Col[Byte]];
      @OverloadId(value = "and_sigma") def &&(other: Rep[Sigma]): Rep[Sigma];
      @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[Sigma];
      @OverloadId(value = "or_sigma") def ||(other: Rep[Sigma]): Rep[Sigma];
      @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[Sigma];
      def lazyAnd(other: Rep[Thunk[Sigma]]): Rep[Sigma];
      def lazyOr(other: Rep[Thunk[Sigma]]): Rep[Sigma]
    };
    trait SigmaBuilder extends DslBuilder;
    @sigmalang trait ProveDlog extends Sigma {
      def value: Rep[WECPoint]
    };
    trait AnyValue extends Def[AnyValue] {
      def dataSize: Rep[Long]
    };
    @sigmalang trait Box extends DslObject {
      def id: Rep[Col[Byte]];
      def value: Rep[Long];
      def bytes: Rep[Col[Byte]];
      def bytesWithoutRef: Rep[Col[Byte]];
      def propositionBytes: Rep[Col[Byte]];
      def dataSize: Rep[Long];
      def registers: Rep[Col[AnyValue]];
      def deserialize[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]];
      def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]];
      def R0[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(0.asInstanceOf[Int]));
      def R1[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(1.asInstanceOf[Int]));
      def R2[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(2.asInstanceOf[Int]));
      def R3[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(3.asInstanceOf[Int]));
      def R4[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(4.asInstanceOf[Int]));
      def R5[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(5.asInstanceOf[Int]));
      def R6[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(6.asInstanceOf[Int]));
      def R7[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(7.asInstanceOf[Int]));
      def R8[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(8.asInstanceOf[Int]));
      def R9[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(9.asInstanceOf[Int]));
      def tokens: Rep[Col[scala.Tuple2[Col[Byte], Long]]] = this.R2[Col[scala.Tuple2[Col[Byte], Long]]].get
    };
    trait BoxBuilder extends DslBuilder;
    trait AvlTree extends DslObject {
      def startingDigest: Rep[Col[Byte]];
      def keyLength: Rep[Int];
      def valueLengthOpt: Rep[WOption[Int]];
      def maxNumOperations: Rep[WOption[Int]];
      def maxDeletes: Rep[WOption[Int]];
      def dataSize: Rep[Long]
    };
    trait AvlTreeBuilder extends DslBuilder;
    trait Context extends Def[Context] {
      def builder: Rep[SigmaDslBuilder];
      def OUTPUTS: Rep[Col[Box]];
      def INPUTS: Rep[Col[Box]];
      def HEIGHT: Rep[Long];
      def SELF: Rep[Box];
      def LastBlockUtxoRootHash: Rep[AvlTree];
      def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]];
      def deserialize[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]]
    };
    trait ContextBuilder extends DslBuilder;
    @sigmalang trait SigmaContract extends Def[SigmaContract] {
      def builder: Rep[SigmaDslBuilder];
      @NeverInline def Collection[T](items: Rep[T]*): Rep[Col[T]] = delayInvoke;
      def verifyZK(cond: Rep[Thunk[Sigma]]): Rep[Boolean] = this.builder.verifyZK(cond);
      def atLeast(bound: Rep[Int], props: Rep[Col[Sigma]]): Rep[Sigma] = this.builder.atLeast(bound, props);
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = this.builder.allOf(conditions);
      def allZK(conditions: Rep[Col[Sigma]]): Rep[Sigma] = this.builder.allZK(conditions);
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = this.builder.anyOf(conditions);
      def anyZK(conditions: Rep[Col[Sigma]]): Rep[Sigma] = this.builder.anyZK(conditions);
      def PubKey(base64String: Rep[String]): Rep[Sigma] = this.builder.PubKey(base64String);
      def sigmaProp(b: Rep[Boolean]): Rep[Sigma] = this.builder.sigmaProp(b);
      def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = this.builder.blake2b256(bytes);
      def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = this.builder.sha256(bytes);
      def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger] = this.builder.byteArrayToBigInt(bytes);
      def longToByteArray(l: Rep[Long]): Rep[Col[Byte]] = this.builder.longToByteArray(l);
      def proveDlog(g: Rep[WECPoint]): Rep[Sigma] = this.builder.proveDlog(g);
      def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[Sigma] = this.builder.proveDHTuple(g, h, u, v);
      def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean] = this.builder.isMember(tree, key, proof);
      def groupGenerator: Rep[WECPoint] = this.builder.groupGenerator;
      @clause def canOpen(ctx: Rep[Context]): Rep[Boolean];
      def asFunction: Rep[scala.Function1[Context, Boolean]] = fun(((ctx: Rep[Context]) => this.canOpen(ctx)))
    };
    trait SigmaContractBuilder extends DslBuilder;
    trait SigmaDslBuilder extends SigmaBuilder with BoxBuilder with AvlTreeBuilder with ContextBuilder with SigmaContractBuilder {
      def Cols: Rep[ColBuilder];
      def verifyZK(cond: Rep[Thunk[Sigma]]): Rep[Boolean];
      def atLeast(bound: Rep[Int], props: Rep[Col[Sigma]]): Rep[Sigma];
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def allZK(conditions: Rep[Col[Sigma]]): Rep[Sigma];
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def anyZK(conditions: Rep[Col[Sigma]]): Rep[Sigma];
      def PubKey(base64String: Rep[String]): Rep[Sigma];
      def sigmaProp(b: Rep[Boolean]): Rep[Sigma];
      def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]];
      def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]];
      def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger];
      def longToByteArray(l: Rep[Long]): Rep[Col[Byte]];
      def proveDlog(g: Rep[WECPoint]): Rep[Sigma];
      def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[Sigma];
      def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean];
      def groupGenerator: Rep[WECPoint]
    };
    trait DslBuilderCompanion;
    trait DslObjectCompanion;
    trait SigmaCompanion;
    trait SigmaBuilderCompanion;
    trait ProveDlogCompanion;
    trait AnyValueCompanion;
    trait BoxCompanion;
    trait BoxBuilderCompanion;
    trait AvlTreeCompanion;
    trait AvlTreeBuilderCompanion;
    trait ContextCompanion;
    trait ContextBuilderCompanion;
    trait SigmaContractCompanion;
    trait SigmaContractBuilderCompanion;
    trait SigmaDslBuilderCompanion
  }
}