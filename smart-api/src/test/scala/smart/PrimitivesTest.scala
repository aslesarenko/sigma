package smart

import org.scalameter.Context

import scala.util.Try
import scalan.BaseNestedTests

class PrimitivesTest extends BaseNestedTests {
  private def ev[T](e: Example[T]): Either[_, T] =
    try { Right(e.result) } catch { case t: Throwable => Left(t) }

  it("successful on unused let") {
    ev(new Ex1) shouldBe Right(3)
  }

  it("successful on x = y") {
    ev(new Ex2) shouldBe Right(6)
  }

  it("successful on simple get") {
    ev(new Ex3(3)) shouldBe Right(3)
  }

  it("successful on get used further in expr") {
    ev(new Ex4) shouldBe Right(false)
  }

  it("successful on multiple lets") {
    ev(new Ex5) shouldBe Right(true)
  }

  it("successful on multiple lets with expression") {
    ev(new Ex6) shouldBe Right(true)
  }

  it("successful on deep type resolution") {
    ev(new Ex7) shouldBe Right(4)
  }

  it("successful on same value names in different branches") {
    ev(new Ex8) shouldBe Right(4)
  }

  it("successful GET/IS_DEFINED") {
    ev(new Ex10) shouldBe Right(false)
    ev(new Ex11) shouldBe Right(true)
    ev(new Ex12) shouldBe Right(1)
  }

  it("custom type field access") {
    ev(new Ex13) shouldBe Right(5)
  }
}
