
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

unmanagedBase := baseDirectory.value / "libs"

lazy val buildSettings = Seq(
  scalaVersion := "2.12.6",
  organization := "special.sigma",
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xlint:-unused,_",
    "-feature",
    "-Ywarn-adapted-args",
    "-Ywarn-inaccessible",
    "-Ywarn-nullary-override",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:experimental.macros"),
  publishTo := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  },
  // do not publish docs for snapshot versions
  publishArtifact in(Compile, packageDoc) := !version.value.trim.endsWith("SNAPSHOT"))

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    // TODO separate benchmark configuration, see https://github.com/scalameter/scalameter-examples/blob/master/basic-with-separate-config/build.sbt
    "com.storm-enroute" %% "scalameter" % "0.10" % Test),
  parallelExecution in Test := false,
  baseDirectory in Test := file("."),
  publishArtifact in Test := true,
  publishArtifact in(Test, packageSrc) := true,
  publishArtifact in(Test, packageDoc) := false,
  test in assembly := {})

lazy val commonSettings = buildSettings ++ testSettings
def libraryDefSettings = commonSettings ++ Seq(
  scalacOptions ++= Seq(
//    s"-Xplugin:${file(".").absolutePath }/scalanizer/target/scala-2.12/scalanizer-assembly-0.3.0-SNAPSHOT.jar"
  )
)

lazy val allConfigDependency = "compile->compile;test->test"
cancelable in Global := true

val paradise = "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full
val common = "special" %% "common" % "0.3.0-SNAPSHOT"
val meta = "special" %% "meta" % "0.3.0-SNAPSHOT"
val core = "special" %% "core" % "0.3.0-SNAPSHOT"
val plugin = "special" %% "plugin" % "0.3.0-SNAPSHOT"
val libraryapi = "special" %% "library-api" % "0.3.0-SNAPSHOT"
val libraryimpl = "special" %% "library-impl" % "0.3.0-SNAPSHOT"
val library= "special" %% "library" % "0.3.0-SNAPSHOT"
val libraryconf = "special" %% "library-conf" % "0.3.0-SNAPSHOT"

lazy val sigmaconf = Project("sigma-conf", file("sigma-conf"))
    .settings(commonSettings,
      libraryDependencies ++= Seq(
        plugin, libraryconf
      ))

lazy val scalanizer = Project("scalanizer", file("scalanizer"))
    .dependsOn(sigmaconf)
    .settings(commonSettings,
      libraryDependencies ++= Seq(meta, plugin, libraryapi, libraryimpl),
      publishArtifact in(Compile, packageBin) := false,
      assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
      artifact in(Compile, assembly) := {
        val art = (artifact in(Compile, assembly)).value
        art.copy(classifier = Some("assembly"))
      },
      addArtifact(artifact in(Compile, assembly), assembly)
    )

lazy val sigmaapi = Project("sigma-api", file("sigma-api"))
    .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        common % allConfigDependency, meta, libraryapi,
        "org.typelevel" %% "macro-compat" % "1.1.1"
      ))

lazy val sigmaimpl = Project("sigma-impl", file("sigma-impl"))
    .dependsOn(sigmaapi % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq(libraryapi, libraryimpl))

lazy val sigmalibrary = Project("sigma-library", file("sigma-library"))
    .dependsOn(sigmaimpl)
    .settings(//commonSettings,
      libraryDefSettings,
      libraryDependencies ++= Seq(
        common % allConfigDependency, core % allConfigDependency, libraryapi,  libraryimpl, library
      ))

lazy val root = Project("sigma", file("."))
    .aggregate(sigmaapi, sigmaimpl, sigmalibrary, sigmaconf, scalanizer)
    .settings(buildSettings, publishArtifact := false)



