import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "BBCPodcastRenamer",
    libraryDependencies += scalaTest % Test
  )
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies ++= Seq("org" % "jaudiotagger" % "2.0.3",
      "io.spray" %%  "spray-json" % "1.3.5",
      "org.scalactic" %% "scalactic" % "3.0.7",
      "commons-io" % "commons-io" % "2.5",
      "org.scalamock" %% "scalamock" % "4.2.0" % Test,
      "com.github.pathikrit" %% "better-files" % "3.7.1")
parallelExecution in Test := false
