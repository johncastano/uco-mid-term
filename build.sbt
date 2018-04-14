import Dependencies._

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "fp.uco",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT"
    )
  ),
  name := "banking-app",
  libraryDependencies += scalaTest % Test
)
scalafmtOnCompile in ThisBuild := true
