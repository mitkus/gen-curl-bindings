import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.lightbend",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "gen-curl-bindings",
    libraryDependencies += jcpp,
    libraryDependencies += scalaTest % Test
  )
