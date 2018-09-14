package special.sigma

import special.collection._

import scala.reflect.ClassTag
import scalan.{NeverInline, Reified}

class CostedOption[T](
    val value: Option[T],
    val left: Costed[Unit],
    val right: Costed[Unit]) extends ConcreteCosted[Option[T]]
{
  @NeverInline
  def cost: Int = left.cost max right.cost + builder.ConstructSumCost
  @NeverInline
  def dataSize: Long = left.dataSize max right.dataSize + builder.SumTagSize
}


trait CostedSigmaObject[TObj] extends ConcreteCosted[TObj] {
  override def builder = new SigmaDslCostedBuilder
  def Operations: CostModel = builder.dsl.CostModel
  def costBoxes(bs: Col[Box]): CostedCol[Box] = {
    val len = bs.length
    val perItemCost = Operations.AccessBox
    val costs = builder.dsl.Cols.replicate(len, perItemCost)
    val sizes = bs.map(b => b.dataSize)
    val valuesCost = Operations.CollectionConst
    new CostedCol(bs, costs, sizes, valuesCost)
  }
  /** Cost of collection with static size elements. */
  def costColWithConstSizedItem[T](xs: Col[T], itemSize: Long): CostedCol[T] = {
    val len = xs.length
    val perItemCost = len * itemSize.toInt / Operations.AccessKiloByteOfData
    val costs = builder.dsl.Cols.replicate(len, perItemCost)
    val sizes = builder.dsl.Cols.replicate(len, itemSize)
    val valueCost = Operations.CollectionConst
    new CostedCol(xs, costs, sizes, valueCost)
  }
  def costOption[T](opt: Option[T], opCost: Int): CostedOption[T] = {
    val left = new CostedPrim((), opCost, 0L)
    val right = new CostedPrim((), opCost, Operations.dataSize(opt))
    new CostedOption[T](opt, left, right)
  }
}

class CostedContext(val ctx: Context) extends ConcreteCosted[Context] with CostedSigmaObject[Context] {
  def OUTPUTS: CostedCol[Box] = this.costBoxes(ctx.OUTPUTS)
  def INPUTS: CostedCol[Box] = this.costBoxes(ctx.INPUTS)
  def HEIGHT: Costed[Long] = {
    val cost = Operations.SelectField
    new CostedPrim(ctx.HEIGHT, cost, 8L)
  }
  def SELF: CostedBox = new CostedBox(ctx.SELF)
  def LastBlockUtxoRootHash: CostedAvlTree = new CostedAvlTree(ctx.LastBlockUtxoRootHash)

  def getVar[T](id: Byte)(implicit cT: ClassTag[T]): CostedOption[T] = {
    val opt = ctx.getVar(id)(cT)
    costOption(opt, Operations.GetVar)
  }
  def deserialize[T](id: Byte)(implicit cT: ClassTag[T]): CostedOption[T] = {
    val opt = ctx.deserialize(id)(cT)
    costOption(opt, Operations.DeserializeVar)
  }

  def value = ctx
  def cost = ctx.cost
  def dataSize = ctx.dataSize
}

class CostedBox(box: Box) extends ConcreteCosted[Box] with CostedSigmaObject[Box] {
  def id: CostedCol[Byte] = costColWithConstSizedItem(box.id, 1)
  def valueCosted: Costed[Long] = {
    val cost = Operations.SelectField
    new CostedPrim(box.value, cost, 8L)
  }
  def bytes: CostedCol[Byte] = costColWithConstSizedItem(box.bytes, 1)
  def bytesWithoutRef: CostedCol[Byte] = costColWithConstSizedItem(box.bytesWithoutRef, 1)
  def propositionBytes: CostedCol[Byte] = costColWithConstSizedItem(box.propositionBytes, 1)
  def registers: CostedCol[AnyValue] = {
    val len = box.registers.length
    val costs = builder.dsl.Cols.replicate(len, Operations.AccessBox)
    val sizes = box.registers.map(o => o.dataSize)
    new CostedCol(box.registers, costs, sizes, Operations.CollectionConst)
  }
  def deserialize[@Reified T](id: Int)(implicit cT:ClassTag[T]): CostedOption[T] = {
    val opt = box.deserialize(id)(cT)
    costOption(opt, Operations.DeserializeRegister)
  }
  def getReg[@Reified T](id: Int)(implicit cT:ClassTag[T]): CostedOption[T] = {
    val opt = box.getReg(id)(cT)
    costOption(opt, Operations.GetRegister)
  }

  def value = box
  def cost = box.cost
  def dataSize = box.dataSize
}

class CostedAvlTree(tree: AvlTree) extends ConcreteCosted[AvlTree] with CostedSigmaObject[AvlTree] {
  def startingDigest: CostedCol[Byte] = costColWithConstSizedItem(tree.startingDigest, 1)
  def keyLength: Costed[Int] = new CostedPrim(tree.keyLength, Operations.SelectField, 4)
  def valueLengthOpt: CostedOption[Int] = costOption(tree.valueLengthOpt, Operations.SelectField)
  def maxNumOperations: CostedOption[Int] = costOption(tree.maxNumOperations, Operations.SelectField)
  def maxDeletes: CostedOption[Int] = costOption(tree.maxDeletes, Operations.SelectField)

  def value = tree
  def cost = tree.cost
  def dataSize = tree.dataSize
}

class SigmaDslCostedBuilder extends ConcreteCostedBuilder {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
}