import Dependencies._

ThisBuild / scalaVersion     := "2.13.14"
ThisBuild / organization     := "net.jcazevedo"
ThisBuild / organizationName := "jcazevedo"

lazy val root = (project in file("."))
  .settings(
    name              := "icfpc2024",
    scalafmtOnCompile := true,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixOnCompile := true,
    fork              := true,
    libraryDependencies ++= Seq(
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
    )
  )
