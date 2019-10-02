// DO NOT EDIT! This file is auto-generated.
// By default, this file does not do anything.
// If the environment variable METALS_ENABLED has the value 'true',
// then this file enables sbt-metals and sbt-bloop.

libraryDependencies := {
  import Defaults.sbtPluginExtra
  val oldDependencies = libraryDependencies.value
  if (System.getenv("METALS_ENABLED") == "true") {
    val bloopModule = "ch.epfl.scala" % "sbt-bloop" % "1.3.2"
    val metalsModule = "org.scalameta" % "sbt-metals" % "0.7.6"
    val sbtVersion = Keys.sbtBinaryVersion.in(TaskKey[Unit]("pluginCrossBuild")).value
    val scalaVersion = Keys.scalaBinaryVersion.in(update).value
    val bloopPlugin = sbtPluginExtra(bloopModule, sbtVersion, scalaVersion)
    val metalsPlugin = sbtPluginExtra(metalsModule, sbtVersion, scalaVersion)
    List(bloopPlugin, metalsPlugin) ++ oldDependencies.filterNot { dep =>
      (dep.organization == "ch.epfl.scala" && dep.name == "sbt-bloop") ||
      (dep.organization == "org.scalameta" && dep.name == "sbt-metals")
    }
  } else {
    oldDependencies
  }
}
