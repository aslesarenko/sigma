package special.sigma {
  import scalan._

  trait WrappersSpec extends Base { self: SigmaLibrary =>
    abstract class OptionWrapSpec extends WrapSpec {
      def get[A](xs: Rep[WOption[A]]): Rep[A] = xs.get;
      def map[A, B](xs: Rep[WOption[A]], f: Rep[scala.Function1[A, B]]): Rep[WOption[B]] = xs.map[B](f)
    };
    trait OptionWrapSpecCompanion
  }
}