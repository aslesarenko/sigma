package special.sigma.wrappers {
  import scalan._

  trait WrappersSpec extends Base { self: SigmaLibrary =>
    abstract class ECPointWrapSpec extends WrapSpec {
      def getEncoded[A](g: Rep[WECPoint]): Rep[WArray[Byte]] = g.getEncoded(toRep(true.asInstanceOf[Boolean]))
    };
    abstract class SigmaPredefWrapSpec extends WrapSpec {
      def cost(v: Rep[Any]): Rep[Int] = WSigmaPredef.cost(v)
    };
    trait ECPointWrapSpecCompanion;
    trait SigmaPredefWrapSpecCompanion
  }
}