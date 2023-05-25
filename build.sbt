ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "dyplome",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6-1f95fd7"
  )
