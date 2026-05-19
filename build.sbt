ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "risc.framework"
ThisBuild / publishTo    := Some(
  Resolver.file("local-ivy", file(Path.userHome + "/.ivy2/local"))
)

val chiselVersion = "7.0.0"

ThisBuild / scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Xcheckinit",
  "-Ymacro-annotations"
)

lazy val root = (project in file("."))
  .settings(
    name := "vutils",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    ),
  )
