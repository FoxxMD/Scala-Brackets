import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin._

object ScalaBracket extends Build {
  lazy val project = Project("ScalaBracket", file("."),
    settings = releaseSettings ++ Seq(
      name := "ScalaBracket",
      version := "0.1",
      organization := "foxxmd.github.io",
      scalaVersion := "2.11.4",
      libraryDependencies ++=Seq(
        "org.json4s" %% "json4s-jackson" % "3.2.10",
        "org.json4s" %% "json4s-ext" % "3.2.10",
        "org.json4s" %% "json4s-core" % "3.2.10",
        "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
        "com.github.julien-truffaut"  %%  "monocle-core"    % "0.5.1"
      ),
    publishTo := Some("Github" at "https://foxxmd.github.io/maven/scalabracket")
    ))
}

