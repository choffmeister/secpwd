import AssemblyKeys._

name := "secpwd"

organization := "de.choffmeister"

version := "0.0.2"

scalaVersion := "2.10.3"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

mainClass := Some("de.choffmeister.secpwd.Main")

resolvers += "maven.choffmeister.de" at "http://maven.choffmeister.de"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.8",
  "com.jcraft" % "jsch" % "0.1.50",
  "org.rogach" %% "scallop" % "0.9.4",
  "de.choffmeister" %% "securestring" % "0.0.1",
  "junit" % "junit" % "4.11" % "test",
  "org.specs2" %% "specs2" % "2.2.3" % "test"
)

testOptions in Test += Tests.Argument("junitxml", "console")

assemblySettings

packSettings

packMain := Map("secpwd" -> "de.choffmeister.secpwd.Main")

ScctPlugin.instrumentSettings

CoveragePlugin.coverageSettings

EclipseKeys.withSource := true
