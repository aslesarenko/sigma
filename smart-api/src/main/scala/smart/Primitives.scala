package smart

import scodec.bits.ByteVector

import scalan.Entity

trait SmartType { type Underlying }
trait NOTHING              extends SmartType { type Underlying = Nothing         }
trait UNIT                 extends SmartType { type Underlying = Unit            }
trait INT                  extends SmartType { type Underlying = Int                  }
trait BYTEVECTOR           extends SmartType { type Underlying = ByteVector           }
trait BOOLEAN              extends SmartType { type Underlying = Boolean              }
trait OPTION[T <: SmartType] extends SmartType { type Underlying = Option[T#Underlying] }

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

trait Example1 extends Example[Int] {
  val X = 2
  def result = 3
}


