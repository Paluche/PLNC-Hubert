scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
                "com.netflix.rxjava" % "rxjava-scala" % "0.18.3")

resolvers += "releases"  at "http://oss.sonatype.org/content/repositories/releases"

scalacOptions ++= Seq("-deprecation", "-feature")

//"org.specs2" %% "specs2" % "2.3.12" % "test",
//                "org.scalatest" % "scalatest_2.11" % "2.1.6" % "test",
