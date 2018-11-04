package special.sigma

import special.SpecialPredef
import special.collection._

import scala.reflect.ClassTag
import scalan.meta.RType
import scalan.{NeverInline, Reified}

class CCostedContext(val ctx: Context) extends CostedContext {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def OUTPUTS: CostedCol[Box] = this.costBoxes(ctx.OUTPUTS)
  def INPUTS: CostedCol[Box] = this.costBoxes(ctx.INPUTS)
  def HEIGHT: Costed[Long] = {
    val cost = Operations.SelectField
    new CCostedPrim(ctx.HEIGHT, cost, 8L)
  }
  def SELF: CostedBox = new CCostedBox(ctx.SELF, Operations.AccessBox)
  def LastBlockUtxoRootHash: CostedAvlTree = new CCostedAvlTree(ctx.LastBlockUtxoRootHash, Operations.AccessAvlTree)

  def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T] = {
    val opt = ctx.getVar(id)(cT)
    costOption(opt, Operations.GetVar)
  }
  def value = ctx
  def cost = ctx.cost
  def dataSize = ctx.dataSize
}

class CCostedBox(box: Box, val cost: Int) extends CostedBox {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def id: CostedCol[Byte] = costColWithConstSizedItem(box.id, 1)
  def valueCosted: Costed[Long] = {
    val cost = Operations.SelectField
    new CCostedPrim(box.value, cost, 8L)
  }
  def bytes: CostedCol[Byte] = costColWithConstSizedItem(box.bytes, 1)
  def bytesWithoutRef: CostedCol[Byte] = costColWithConstSizedItem(box.bytesWithoutRef, 1)
  def propositionBytes: CostedCol[Byte] = costColWithConstSizedItem(box.propositionBytes, 1)
  def registers: CostedCol[AnyValue] = {
    val len = box.registers.length
    val costs = dsl.Cols.replicate(len, Operations.AccessBox)
    val sizes = box.registers.map(o => o.dataSize)
    new CCostedCol(box.registers, costs, sizes, Operations.CollectionConst)
  }
  def deserialize[@Reified T](id: Int)(implicit cT: RType[T]): CostedOption[T] = {
    val opt = box.deserialize(id)(cT)
    costOption(opt, Operations.DeserializeRegister)
  }
  def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T] = {
    val opt = box.getReg(id)(cT)
    costOption(opt, Operations.GetRegister)
  }

  def value = box
  def dataSize = box.dataSize
}

class CCostedAvlTree(val tree: AvlTree, val cost: Int) extends CostedAvlTree {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def startingDigest: CostedCol[Byte] = costColWithConstSizedItem(tree.startingDigest, 1)
  def keyLength: Costed[Int] = new CCostedPrim(tree.keyLength, Operations.SelectField, 4)
  def valueLengthOpt: CostedOption[Int] = costOption(tree.valueLengthOpt, Operations.SelectField)
  def maxNumOperations: CostedOption[Int] = costOption(tree.maxNumOperations, Operations.SelectField)
  def maxDeletes: CostedOption[Int] = costOption(tree.maxDeletes, Operations.SelectField)

  def value = tree
  def dataSize = tree.dataSize
}

