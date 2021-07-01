val scala3Version = "3.0.0"

// library name
name := "wtul-rosters"

// library version
version := "0.1.0"

unmanagedSources / excludeFilter := ".#*"
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
  // .dependsOn(RootProject(file("/home/jm/Lib/Scala/Utils")))
  .dependsOn(RootProject(file("/home/jm/Lib/Scala/LaTeX")))
