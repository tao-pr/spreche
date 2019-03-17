name         := "spreche"
organization := "com.starcolon"
description  := "Learn to speak German!"
homepage     := Some(url("https://github.com/starcolon/spreche"))

version := "0.0.1-SNAPSHOT"

scalaVersion   := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

libraryDependencies ++= Seq()

val devDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

lazy val spreche = project
  .settings(libraryDependencies ++= devDependencies)

