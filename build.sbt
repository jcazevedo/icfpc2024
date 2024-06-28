import Dependencies._

ThisBuild / scalaVersion     := "2.13.14"
ThisBuild / organization     := "net.jcazevedo"
ThisBuild / organizationName := "jcazevedo"

lazy val root = (project in file("."))
  .settings(
    name                             := "icfpc2024",
    scalafmtOnCompile                := true,
    semanticdbEnabled                := true,
    semanticdbVersion                := scalafixSemanticdb.revision,
    scalafixOnCompile                := true,
    fork                             := true,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:imports"
    ),
    libraryDependencies ++= Seq(
      JLine,
      Log4jCore,
      Log4jSlf4jImpl,
      PekkoActor,
      PekkoHttp,
      PekkoSlf4j,
      PekkoStream,
      PureConfigCore,
      PureConfigGeneric,
      PureConfigPekkoHttp,
      ScalaLogging,
      MUnit % Test
    ),
    assembly / mainClass             := Some("net.jcazevedo.icfpc2024.CLI"),
    assembly / assemblyJarName       := name.value + "-" + version.value + ".jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case "reference.conf"                    => MergeStrategy.concat
      case _                                   => MergeStrategy.first
    }
  )
