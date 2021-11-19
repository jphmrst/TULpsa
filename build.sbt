val scala3Version = "3.1.0"

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
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.maraist" %% "scala-latex" % "2.0.1"
//        ,
//      "org.maraist" %% "scala-outlines" % "2.0.0"
    )
  ).dependsOn(RootProject(file("/home/jm/Lib/Scala/Outlines/")))
   // .dependsOn(RootProject(file("/home/jm/Lib/Scala/LaTeX/")))
