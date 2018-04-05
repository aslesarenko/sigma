package spu.device.config

import special.library.config.SpecialLibraryConfig

import scalan.meta.{LibraryConfig, SourceModuleConf}

class SmartLibraryConfig extends LibraryConfig {
  def name = "smart"
  def baseDir = ""
  val specialLibrary = new SpecialLibraryConfig

  val ApiModule = new SourceModuleConf("", "smart-api")
      .moduleDependencies(specialLibrary.ApiModule)
      .addUnit("Types.scala", "special/smart/Types.scala")
      .addUnit("Examples.scala", "special/smart/Examples.scala")
      .addUnit("SigmaTypes.scala", "special/smart/SigmaTypes.scala")
      .addUnit("SigmaExamples.scala", "special/smart/SigmaExamples.scala")

  def sourceModules = List(ApiModule)
  def targetModules = Nil
}
