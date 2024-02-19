ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "scala_crawler",
    libraryDependencies ++= Seq(
      "org.jsoup" % "jsoup" % "1.14.3",
      "org.json4s" %% "json4s-native" % "4.0.3"
    )
  )
