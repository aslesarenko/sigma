package spu.device.config

import special.library.config.SpecialLibraryConfig

import scalan.meta.{LibraryConfig, ConfMap, TargetModuleConf, SourceModuleConf}

class SigmaLibraryConfig extends LibraryConfig {
  def name = "sigma"
  def baseDir = ""
  val specialLibrary = new SpecialLibraryConfig

  val ApiModule: SourceModuleConf = new SourceModuleConf(baseDir, "sigma-api")
      .moduleDependencies(specialLibrary.ApiModule)
      .addUnit("SigmaDsl.scala", "special/sigma/SigmaDsl.scala")
      .addUnit("SigmaExamples.scala", "special/sigma/SigmaExamples.scala")

  val ImplModule = new SourceModuleConf(baseDir, "sigma-impl")
      .moduleDependencies(specialLibrary.ApiModule, specialLibrary.ImplModule)
      .addUnit("SigmaDslOverArrays.scala", "special/sigma/SigmaDslOverArrays.scala")
      .dependsOn(ApiModule)

  val TargetModule = new TargetModuleConf(baseDir, "sigma-library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule))

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(TargetModule)
}
