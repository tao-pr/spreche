name         := "spreche"
organization := "com.starcolon"
description  := "Learn to speak German!"
homepage     := Some(url("https://github.com/starcolon/spreche"))

version := "0.0.1-SNAPSHOT"

scalaVersion   := "2.11.12"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

val addtionalDependencies = Seq(
  "org.json4s" %% "json4s-jackson" % "3.6.5"
)

val devDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

lazy val spreche = project
  .settings(libraryDependencies ++= devDependencies ++ addtionalDependencies)

