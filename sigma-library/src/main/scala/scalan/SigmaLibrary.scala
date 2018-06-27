package scalan

import special.sigma.{SigmaDslOverArraysModule, SigmaDslModule, SigmaExamplesModule}

trait SigmaLibrary extends Library
  with SigmaDslModule
  with SigmaExamplesModule
  with SigmaDslOverArraysModule {

}
