package smart

import scodec.bits.ByteVector

import scalan.Entity

//trait SmartType { type Underlying }
//trait NOTHING              extends SmartType { type Underlying = Nothing         }
//trait UNIT                 extends SmartType { type Underlying = Unit            }
//trait INT                  extends SmartType { type Underlying = Int                  }
//trait BYTEVECTOR           extends SmartType { type Underlying = ByteVector           }
//trait BOOLEAN              extends SmartType { type Underlying = Boolean              }
//trait OPTION[T <: SmartType] extends SmartType { type Underlying = Option[T#Underlying] }

@Entity
trait Transaction {
  def TYPE: Int
  def ID: ByteVector
  def BODYBYTES: ByteVector
  def SENDERPK: ByteVector
  def PROOFA: ByteVector
  def PROOFB: ByteVector
  def ROOFC: ByteVector
  def ASSETID: Option[ByteVector]
}

@Entity
trait Context {
  def H: Int
  def TX: Transaction
}

trait Example[R] {
  def result: R
}

class Ex1 extends Example[Int] {
  def result = {
    val x = 2
    3
  }
//  expr = Block(
//    Some(LET("x", CONST_INT(3))),
//    CONST_INT(3)
//  )
}

class Ex2 extends Example[Int] {
  def result = {
    val x = 3
    val y = x
    x + y
  }
  //      expr = Block(Some(LET("x", CONST_INT(3))),
  //        Block(
  //          Some(LET("y", REF("x"))),
  //          SUM(REF("x"), REF("y"))
  //        ))
}

class Ex3(i: Int) extends Example[Int] {
  def result = {
    val x = i
    x
  }
  // Block(Some(LET("x", CONST_INT(i))), REF("x"))
}

class Ex4 extends Example[Boolean] {
  def result = {
    val x = 3
    x == 2
  }
//  expr = Block(
//    Some(LET("x", CONST_INT(3))),
//    EQ(REF("x"), CONST_INT(2))
//  )
}

class Ex5 extends Example[Boolean] {
  def result = {
    val x = 3
    val y = 3
    x == y
  }
//  expr = Block(
//    Some(LET("x", CONST_INT(3))),
//    Block(Some(LET("y", CONST_INT(3))), EQ(REF("x"), REF("y")))
//  )
}
class Ex6 extends Example[Boolean] {
  def result = {
    val x = 3
    val y = 3 + 0
    x == y
  }
  //      expr = Block(
  //        Some(LET("x", CONST_INT(3))),
  //        Block(Some(LET("y", SUM(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), REF("y")))
  //      )
}
class Ex7 extends Example[Int] {
  def result = {
    if (1 == 2) { new Ex3(3).result } else 4
  }
//  expr = IF(EQ(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), CONST_INT(4))
}

class Ex8 extends Example[Int] {
  def result = {
    if (1 == 2) { new Ex3(3).result } else { new Ex3(4).result }
  }
//  expr = IF(EQ(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4))
}

class Ex10 extends Example[Boolean] {
  def result = None.isDefined
//  ev(expr = IS_DEFINED(NONE)) shouldBe Right(false)
}

class Ex11 extends Example[Boolean] {
  def result = Some(1).isDefined
//  ev(expr = IS_DEFINED(SOME(CONST_INT(1)))) shouldBe Right(true)
}

class Ex12 extends Example[Int] {
  def result = Some(1).get
//  ev(expr = GET(SOME(CONST_INT(1)))) shouldBe Right(1)
}

class Point(val X: Int, Y: Int)

class Ex13 extends Example[Int] {
  val p = new Point(X = 3, Y = 4)
  def result = p.X + 2
  //  ev(
  //  predefTypes = Map(pointType.name -> pointType),
  //  defs = Map("p" -> (TYPEREF("Point"), pointInstance)),
  //  expr = SUM(GETTER(REF("p"), "X"), CONST_INT(2))
  //  )
}


