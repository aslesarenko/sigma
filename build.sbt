import scala.util.Try

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

unmanagedBase := baseDirectory.value / "libs"

lazy val buildSettings = Seq(
  scalaVersion := "2.12.6",
  organization := "io.github.scalan",
  resolvers += Resolver.sonatypeRepo("public"),
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
    //    val nexus = "http://10.122.85.37:9081/nexus/"
    val nexus = "https://oss.sonatype.org/"
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
//    s"-Xplugin:${file(".").absolutePath }/scalanizer/target/scala-2.12/scalanizer-assembly-optimizations-51cf49fb-SNAPSHOT.jar"
  )
)

lazy val allConfigDependency = "compile->compile;test->test"
cancelable in Global := true

val specialVersion = "master-4e1b2bdb-SNAPSHOT"

val scripto     = "org.scorexfoundation" %% "scrypto" % "2.1.0"
val paradise    = "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full
val common      = "io.github.scalan" %% "common" % specialVersion
val meta        = "io.github.scalan" %% "meta" % specialVersion
val core        = "io.github.scalan" %% "core" % specialVersion
val plugin      = "io.github.scalan" %% "plugin" % specialVersion
val libraryapi  = "io.github.scalan" %% "library-api" % specialVersion
val libraryimpl = "io.github.scalan" %% "library-impl" % specialVersion
val library     = "io.github.scalan" %% "library" % specialVersion
val libraryconf = "io.github.scalan" %% "library-conf" % specialVersion

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
        art.withClassifier(Some("assembly"))
      },
      addArtifact(artifact in(Compile, assembly), assembly)
    )

lazy val sigmaapi = Project("sigma-api", file("sigma-api"))
    .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        common, meta, libraryapi,
        "org.typelevel" %% "macro-compat" % "1.1.1",
        "org.scorexfoundation" %% "scrypto" % "2.1.2",
        "org.bouncycastle" % "bcprov-jdk15on" % "1.60"
      ))

lazy val sigmaimpl = Project("sigma-impl", file("sigma-impl"))
    .dependsOn(sigmaapi % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq(
        libraryapi, libraryimpl,
        scripto,
        "org.bouncycastle" % "bcprov-jdk15on" % "1.60"
      ))

lazy val sigmalibrary = Project("sigma-library", file("sigma-library"))
    .dependsOn(sigmaimpl % allConfigDependency)
    .settings(//commonSettings,
      libraryDefSettings,
      libraryDependencies ++= Seq(
        common, (common % Test).classifier("tests"),
        core, (core % Test).classifier("tests"),
        libraryapi, (libraryapi % Test).classifier("tests"),
        libraryimpl, (libraryimpl % Test).classifier("tests"),
        library, (library % Test).classifier("tests"),
        "org.scorexfoundation" %% "scrypto" % "2.1.2",
        "org.bouncycastle" % "bcprov-jdk15on" % "1.60"
      ))

lazy val root = Project("sigma", file("."))
    .aggregate(sigmaapi, sigmaimpl, sigmalibrary, sigmaconf, scalanizer)
    .settings(buildSettings, publishArtifact := false)

enablePlugins(GitVersioning)

version in ThisBuild := {
  if (git.gitCurrentTags.value.nonEmpty) {
    git.gitDescribedVersion.value.get
  } else {
    if (git.gitHeadCommit.value.contains(git.gitCurrentBranch.value)) {
      // see https://docs.travis-ci.com/user/environment-variables/#default-environment-variables
      if (Try(sys.env("TRAVIS")).getOrElse("false") == "true") {
        // pull request number, "false" if not a pull request
        if (Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false") {
          // build is triggered by a pull request
          val prBranchName = Try(sys.env("TRAVIS_PULL_REQUEST_BRANCH")).get
          val prHeadCommitSha = Try(sys.env("TRAVIS_PULL_REQUEST_SHA")).get
          prBranchName + "-" + prHeadCommitSha.take(8) + "-SNAPSHOT"
        } else {
          // build is triggered by a push
          val branchName = Try(sys.env("TRAVIS_BRANCH")).get
          branchName + "-" + git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
        }
      } else {
        git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
      }
    } else {
      git.gitCurrentBranch.value + "-" + git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
    }
  }
}

git.gitUncommittedChanges in ThisBuild := true

credentials += Credentials(Path.userHome / ".sbt" / ".specialsigma-sonatype-credentials")

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

