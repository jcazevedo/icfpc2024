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
    libraryDependencies ++= Seq(PekkoActor, PekkoHttp, PekkoStream, PureConfigCore, PureConfigGeneric, MUnit % Test)
  )
