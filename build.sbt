name := "scala_beyond_basics"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

fork := true

libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.0.0" % "test",
                            "org.scalatest" %% "scalatest" % "3.0.0" % "test")
