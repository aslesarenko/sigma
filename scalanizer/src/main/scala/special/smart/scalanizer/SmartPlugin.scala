package special.smart.scalanizer

import spu.device.config.SmartLibraryConfig

import scala.tools.nsc.Global
import scalan.meta.{ConfMap, TargetModuleConf, SourceModuleConf}
import scalan.meta.scalanizer.ScalanizerConfig
import scalan.plugin.{ScalanizerPluginConfig, ScalanizerPlugin}

class SmartPlugin(g: Global) extends ScalanizerPlugin(g) { plugin =>
  override def createScalanizerConfig(): ScalanizerConfig = new SmartScalanizerConfig
}

class SmartScalanizerConfig extends ScalanizerPluginConfig {
  val smart = new SmartLibraryConfig()

  /** Modules that contain units to be virtualized by scalan-meta. */
  override val sourceModules: ConfMap[SourceModuleConf] = ConfMap(smart.sourceModules: _*)
  override val targetModules: ConfMap[TargetModuleConf] = ConfMap(smart.targetModules: _*)
}
