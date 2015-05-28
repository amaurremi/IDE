name := "Correlated Calls"

version := "0.1"

organization := "University of Waterloo"

scalaVersion := "2.11.2"

resolvers += "Local Maven Repository" at "file:///"+Path.userHome.absolutePath+"/.m2/repository"

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.+",
  "com.typesafe" % "config" % "0.5.+",
  "com.ibm.wala" % "com.ibm.wala.shrike" % "1.3.4-SNAPSHOT",
  "com.ibm.wala" % "com.ibm.wala.util" % "1.3.4-SNAPSHOT",
  "com.ibm.wala" % "com.ibm.wala.core" % "1.3.4-SNAPSHOT",
  "com.ibm.wala" % "com.ibm.wala.core.tests" % "1.3.4-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test",
  "org.mockito" % "mockito-core" % "1.9.5",
  "org.scalaz" %% "scalaz-core" % "7.1.0")

org.scalastyle.sbt.ScalastylePlugin.Settings
