package special.ivylang

import scala.annotation.Annotation
import scalan.lang

@lang("ivylang")
class ivy extends Annotation {}

@ivy class clause extends Annotation
