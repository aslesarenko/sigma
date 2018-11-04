package special.sigma

import special.SpecialPredef
import special.collection._

import scalan.Reified
import scalan.meta.RType

trait CostedSigmaObject[Val] extends Costed[Val] {
  def dsl: SigmaDslBuilder
  def builder: CostedBuilder = dsl.Costing
  def Operations: CostModel = dsl.CostModel
  def costBoxes(bs: Col[Box]): CostedCol[Box] = {
    val len = bs.length
    val perItemCost = Operations.AccessBox
    val costs = dsl.Cols.replicate(len, perItemCost)
    val sizes = bs.map(b => b.dataSize)
    val valuesCost = Operations.CollectionConst
    dsl.Costing.mkCostedCol(bs, costs, sizes, valuesCost)
  }
  /** Cost of collection with static size elements. */
  def costColWithConstSizedItem[T](xs: Col[T], itemSize: Long): CostedCol[T] = {
    val len = xs.length
    val perItemCost = len * itemSize.toInt / Operations.AccessKiloByteOfData
    val costs = dsl.Cols.replicate(len, perItemCost)
    val sizes = dsl.Cols.replicate(len, itemSize)
    val valueCost = Operations.CollectionConst
    dsl.Costing.mkCostedCol(xs, costs, sizes, valueCost)
  }
  def costOption[T](opt: Option[T], opCost: Int)(implicit cT: RType[T]): CostedOption[T] = {
    val none = dsl.Costing.mkCostedNone(opCost)
    opt.fold[CostedOption[T]](none)(x => dsl.Costing.mkCostedSome(dsl.Costing.costedValue(x, SpecialPredef.some(opCost))))
  }
}

trait CostedContext extends CostedSigmaObject[Context] {
  def OUTPUTS: CostedCol[Box]
  def INPUTS: CostedCol[Box]
  def HEIGHT: Costed[Long]
  def SELF: CostedBox
  def LastBlockUtxoRootHash: CostedAvlTree
  def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T]
  def value: Context
  def cost: Int
  def dataSize: Long
}

trait CostedBox extends CostedSigmaObject[Box] {
  def id: CostedCol[Byte]
  def valueCosted: Costed[Long]
  def bytes: CostedCol[Byte]
  def bytesWithoutRef: CostedCol[Byte]
  def propositionBytes: CostedCol[Byte]
  def registers: CostedCol[AnyValue]
  def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T]
}

trait CostedAvlTree extends CostedSigmaObject[AvlTree] {
  def startingDigest: CostedCol[Byte]
  def keyLength: Costed[Int]
  def valueLengthOpt: CostedOption[Int]
  def maxNumOperations: CostedOption[Int]
  def maxDeletes: CostedOption[Int]
}
