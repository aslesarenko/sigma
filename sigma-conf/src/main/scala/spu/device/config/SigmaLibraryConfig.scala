package spu.device.config

import special.library.config.SpecialLibraryConfig

import scalan.meta.{LibraryConfig, SourceModuleConf, ConfMap, TargetModuleConf}

class SigmaLibraryConfig extends LibraryConfig {
  def name = "sigma"
  def baseDir = ""
  val specialLibrary = new SpecialLibraryConfig

  val ApiModule = new SourceModuleConf("", "sigma-api")
      .moduleDependencies(specialLibrary.ApiModule)
      .addUnit("SigmaDsl.scala", "special/sigma/SigmaDsl.scala")
      .addUnit("SigmaExamples.scala", "special/sigma/SigmaExamples.scala")

  val ImplModule = new SourceModuleConf("", "sigma-impl")
      .moduleDependencies(specialLibrary.ApiModule, specialLibrary.ImplModule)
      .addUnit("SigmaDslOverArrays.scala", "special/sigma/SigmaDslOverArrays.scala")
      .dependsOn(ApiModule)

  val TargetModule = new TargetModuleConf("", "sigma-library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule))

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(TargetModule)
}
