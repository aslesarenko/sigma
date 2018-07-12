package special.sigma.wrappers

import wrappers.org.bouncycastle.math.ec.WECPointsModule
import special.sigma.WSigmaPredefsModule

trait WrappersModule
  extends scala.wrappers.WrappersModule
     with WSigmaPredefsModule
     with WECPointsModule
