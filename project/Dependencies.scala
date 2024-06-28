import sbt._

object Dependencies {
  lazy val MUnit       = "org.scalameta"    %% "munit"        % "0.7.29"
  lazy val PekkoActor  = "org.apache.pekko" %% "pekko-actor"  % "1.0.2"
  lazy val PekkoHttp   = "org.apache.pekko" %% "pekko-http"   % "1.0.1"
  lazy val PekkoStream = "org.apache.pekko" %% "pekko-stream" % "1.0.2"
}
