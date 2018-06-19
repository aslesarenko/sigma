package special.sigma

import scalan.collection.ColOverArrayBuilder

class ContextOverArrays(val inputs: Array[Box], val outputs: Array[Box], val HEIGHT: Long) extends Context {
  def builder = new ContextOverArrayBuilder

  def OUTPUTS = ???

  def INPUTS = ???
}

class ContextOverArrayBuilder extends ContextBuilder {
  def Collections = new ColOverArrayBuilder
}
