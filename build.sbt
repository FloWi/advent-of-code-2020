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
    scalaVersion := "2.13.4"
  )
