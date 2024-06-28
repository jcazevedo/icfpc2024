import sbt._

object Dependencies {
  lazy val JLine               = "org.jline"                   % "jline"                 % "3.1.3"
  lazy val Log4jCore           = "org.apache.logging.log4j"    % "log4j-core"            % "2.19.0"
  lazy val Log4jSlf4jImpl      = "org.apache.logging.log4j"    % "log4j-slf4j-impl"      % "2.23.1"
  lazy val MUnit               = "org.scalameta"              %% "munit"                 % "0.7.29"
  lazy val PekkoActor          = "org.apache.pekko"           %% "pekko-actor"           % "1.0.2"
  lazy val PekkoHttp           = "org.apache.pekko"           %% "pekko-http"            % "1.0.1"
  lazy val PekkoSlf4j          = "org.apache.pekko"           %% "pekko-slf4j"           % "1.0.2"
  lazy val PekkoStream         = "org.apache.pekko"           %% "pekko-stream"          % "1.0.2"
  lazy val PureConfigCore      = "com.github.pureconfig"      %% "pureconfig-core"       % "0.17.7"
  lazy val PureConfigGeneric   = "com.github.pureconfig"      %% "pureconfig-generic"    % "0.17.7"
  lazy val PureConfigPekkoHttp = "com.github.pureconfig"      %% "pureconfig-pekko-http" % "0.17.7"
  lazy val ScalaLogging        = "com.typesafe.scala-logging" %% "scala-logging"         % "3.9.4"
}
