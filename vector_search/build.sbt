import scala.collection.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "vector_search",
    libraryDependencies ++= Seq(
      "edu.stanford.nlp" % "stanford-corenlp" % "4.2.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "4.2.0" classifier "models",
      "edu.stanford.nlp" % "stanford-corenlp" % "4.2.0" classifier "models-english",
      "edu.stanford.nlp" % "stanford-corenlp" % "4.2.0" classifier "models-english-kbp",
      "org.jsoup" % "jsoup" % "1.14.3",
      "org.json4s" %% "json4s-native" % "4.0.3",
      "org.apache.lucene" % "lucene-core" % "8.11.0",
      "com.typesafe.akka" %% "akka-http" % "10.5.0",
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.0",
      "com.typesafe.akka" %% "akka-http-spray-json" % "10.5.0",
      "com.typesafe.akka" %% "akka-stream" % "2.8.0"
    )
  )
