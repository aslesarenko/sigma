package special.sigma

import library.WrapSpec

/** Wrappers spec for Option */
class OptionWrapSpec extends WrapSpec {
  def get[A](xs: Option[A]): A = xs.get
  def map[A,B](xs: Option[A], f: A => B): Option[B] = xs.map(f)
};

