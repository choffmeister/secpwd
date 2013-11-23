name := "secpwd"

organization := "de.choffmeister"

version := "0.0.1"

scalaVersion := "2.10.3"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.8",
  "com.jcraft" % "jsch" % "0.1.50",
  "org.rogach" %% "scallop" % "0.9.4",
  "junit" % "junit" % "4.11" % "test",
  "org.specs2" %% "specs2" % "2.2.3" % "test"
)

testOptions in Test += Tests.Argument("junitxml", "console")

packSettings

packMain := Map("secpwd" -> "de.choffmeister.secpwd.Main")

ScctPlugin.instrumentSettings

EclipseKeys.withSource := true
