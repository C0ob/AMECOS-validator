ThisBuild / scalaVersion := "3.8.3"

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("Amecos")
Antlr4 / antlr4GenListener := false
Antlr4 / antlr4GenVisitor := true

libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.8"
libraryDependencies += "org.apache.xmlgraphics" % "batik-all" % "1.17"
libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.9"

lazy val root = (project in file("."))
  .settings(
    name := "AMECOS validator"
  )
