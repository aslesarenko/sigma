package spu.device.config

import special.library.config.SpecialLibraryConfig

import scalan.meta.{LibraryConfig, SourceModuleConf}

class SigmaLibraryConfig extends LibraryConfig {
  def name = "sigma"
  def baseDir = ""
  val specialLibrary = new SpecialLibraryConfig

  val ApiModule = new SourceModuleConf("", "sigma-api")
      .moduleDependencies(specialLibrary.ApiModule)
      .addUnit("SigmaDsl.scala", "special/sigma/SigmaDsl.scala")
      .addUnit("SigmaExamples.scala", "special/sigma/SigmaExamples.scala")

  def sourceModules = List(ApiModule)
  def targetModules = Nil
}
