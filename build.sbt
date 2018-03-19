name := "scala_beyond_basics"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.0.0" % "test",
                            "org.scalatest" %% "scalatest" % "3.0.0" % "test")

EclipseKeys.withSource := true

EclipseKeys.withJavadoc := true

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18)
