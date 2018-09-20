package special.sigma

import special.collection.Col

import scalan.Internal
import scalan.meta.RType

@Internal
trait TypeDescriptors {
  implicit def RTInt: RType[Int] = RType[Int]
  implicit def RTLong: RType[Long] = RType[Long]
  implicit def RTColByte: RType[Col[Byte]] = RType[Col[Byte]]
}
