val scala3Version = "3.0.0"

lazy val utils = RootProject(file("/home/jm/Lib/Scala/Utils"))

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  ).dependsOn(utils)
