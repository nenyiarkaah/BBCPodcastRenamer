import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "BBCPodcastRenamer",
    libraryDependencies += scalaTest % Test
  )
libraryDependencies += "org" % "jaudiotagger" % "2.0.3"
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "commons-io" % "commons-io" % "2.5"
libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % Test
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.0.0"
parallelExecution in Test := false
