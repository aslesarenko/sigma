package scalan

import special.sigma.{SigmaDslOverArraysModule, SigmaDslModule, SigmaExamplesModule, WrappersSpecModule}
import special.sigma.wrappers.WrappersModule

trait SigmaLibrary extends Library
  with WrappersModule
  with WrappersSpecModule
  with SigmaDslModule
  with SigmaExamplesModule
  with SigmaDslOverArraysModule {

}
