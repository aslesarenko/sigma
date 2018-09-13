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

class CostTable {
  def AccessBoxOp: String = "AccessBox: Context => Box"
  def AccessContextVarOp: String = "ContextVar: (Context, Byte) => Option[T]"
  def AccessRegisterOp: String = "AccessRegister: Box => Option[T]"
  def AccessHeightOp: String = "AccessHeight: Context => Long"
  def SelectFieldOp: String = "SelectField"
  def CollectionConstOp: String = "Const: () => Array[IV]"
}

class CostedContext(val ctx: Context) extends ConcreteCosted[Context] {
  override def builder = new SigmaDslCostedBuilder
  def costBoxes(bs: Col[Box]): CostedCol[Box] = {
    val len = bs.length
    val perItemCost = SigmaPredef.costOf(builder.costTable.AccessBoxOp)
    val costs = builder.dsl.Cols.replicate(len, perItemCost)
    val sizes = bs.map(b => b.dataSize)
    val valuesCost = SigmaPredef.costOf(builder.costTable.CollectionConstOp)
    new CostedCol(bs, costs, sizes, valuesCost)
  }
  def OUTPUTS: CostedCol[Box] = this.costBoxes(ctx.OUTPUTS)
  def INPUTS: CostedCol[Box] = this.costBoxes(ctx.INPUTS)
  def HEIGHT: Costed[Long] = {
    val cost = SigmaPredef.costOf(builder.costTable.SelectFieldOp)
    new CostedPrim(ctx.HEIGHT, cost, 8L)
  }
  def SELF: CostedBox = new CostedBox(ctx.SELF)
  def LastBlockUtxoRootHash: CostedAvlTree = new CostedAvlTree(ctx.LastBlockUtxoRootHash)
  def costOption[T](id: Byte)(implicit cT: ClassTag[T]): CostedOption[T] = {
    val value = ctx.getVar(id)(cT)
    val accessVarCost = SigmaPredef.costOf(builder.costTable.AccessContextVarOp)
    val left = new CostedPrim((), accessVarCost, 0L)
    val right = new CostedPrim((), accessVarCost, SigmaPredef.dataSize(value))
    new CostedOption[T](value, left, right)
  }
  def getVar[T](id: Byte)(implicit cT: ClassTag[T]): CostedOption[T] = costOption(id)(cT)
  def deserialize[T](id: Byte)(implicit cT: ClassTag[T]): CostedOption[T] = costOption(id)(cT)

  def value = ctx

  def cost = this.INPUTS.cost + this.OUTPUTS.cost + this.SELF.cost + this.HEIGHT.cost + this.LastBlockUtxoRootHash.cost

  def dataSize = this.INPUTS.dataSize + this.OUTPUTS.dataSize + this.SELF.dataSize + this.HEIGHT.dataSize + this.LastBlockUtxoRootHash.dataSize
}

class CostedBox(box: Box) extends ConcreteCosted[Box] {
  override def builder = new SigmaDslCostedBuilder
  /** Cost of collection with static size elements. */
  def costCol[T](xs: Col[T], elementCost: Int): CostedCol[T] = {
    val len = xs.length
    val costs = builder.dsl.Cols.replicate(len, elementCost)
    val sizes = builder.dsl.Cols.replicate(len, elementCost.toLong)
    new CostedCol(xs, costs, sizes, SigmaPredef.costOf(builder.costTable.CollectionConstOp))
  }
  def id: CostedCol[Byte] = costCol(box.id, 1)
  def value: Costed[Long] = new CostedPrim(box.value, SigmaPredef.costOf(builder.costTable.AccessHeightOp), 8L)
  def bytes: CostedCol[Byte] = costCol(box.bytes, 1)
  def bytesWithoutRef: CostedCol[Byte] = costCol(box.bytesWithoutRef, 1)
  def propositionBytes: CostedCol[Byte] = costCol(box.propositionBytes, 1)
  def registers: CostedCol[AnyValue] = {
    val len = box.registers.length
    val costs = builder.dsl.Cols.replicate(len, SigmaPredef.costOf(builder.costTable.AccessBoxOp))
    val sizes = box.registers.map(o => o.dataSize)
    new CostedCol(box.registers, costs, sizes, SigmaPredef.costOf(builder.costTable.CollectionConstOp))
  }
  def deserialize[@Reified T](i: Int)(implicit cT:ClassTag[T]): CostedOption[T]
  def getReg[@Reified T](i: Int)(implicit cT:ClassTag[T]): CostedOption[T]
}

class CostedAvlTree(tree: AvlTree) extends ConcreteCosted[AvlTree] {
  override def builder = new SigmaDslCostedBuilder
  def startingDigest: CostedCol[Byte]
  def keyLength: Costed[Int]
  def valueLengthOpt: CostedOption[Int]
  def maxNumOperations: CostedOption[Int]
  def maxDeletes: CostedOption[Int]
}

class SigmaDslCostedBuilder extends ConcreteCostedBuilder {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def costTable: CostTable
}