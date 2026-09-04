ThisBuild / scalaVersion := "3.8.3"

enablePlugins(Antlr4Plugin)

antlr4PackageName in Antlr4 := Some("Amecos")
antlr4GenListener in Antlr4 := false
antlr4GenVisitor in Antlr4 := true

libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.8"

lazy val root = (project in file("."))
  .settings(
    name := "AMECOS validator"
  )
