scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
    "com.netflix.rxjava" % "rxjava-scala" % "0.18.3",
    "org.specs2" %% "specs2" % "2.3.12" % "test",
    "org.scalatest" % "scalatest_2.11" % "2.1.6" % "test",
    "io.spray" %%  "spray-json" % "1.2.6")

resolvers ++= Seq(
    "releases"  at "http://oss.sonatype.org/content/repositories/releases",
    "spray" at "http://repo.spray.io/")

scalacOptions ++= Seq("-deprecation", "-feature")

