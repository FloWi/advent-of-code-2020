import sbt.Keys._
import sbt._

//val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file(""))
  .settings(
    name := "de.flwi.adventofcode2020",
    version := "0.1.0",
    // scalacOptions ++= Seq(
    //   "-language:postfixOps",
    //   // "-Ykind-projector",
    //   //"-Yexplicit-nulls",
    //   "-source",
    //   "3.1"
    // ),
    scalaVersion := "2.13.4",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.2.0",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
  )
