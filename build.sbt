
// Building both for JVM and JavaScript runtimes.

// To convince SBT not to publish any root level artifacts, I had a look at how scala-java-time does it.
// See https://github.com/cquiroz/scala-java-time/blob/master/build.sbt as a "template" for this build file.

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaVer = "2.13.0"

val crossScalaVer = Seq(scalaVer)

lazy val commonSettings = Seq(
  name         := "introchemistry",
  description  := "Stoichiometry support in the Duke University course Introduction to Chemistry",
  organization := "eu.cdevreeze.introchemistry",
  version      := "0.1.0-SNAPSHOT",

  scalaVersion       := scalaVer,
  crossScalaVersions := crossScalaVer,

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint", "-target:jvm-1.8"),

  Test / publishArtifact := false,
  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

  pomExtra := pomData,
  pomIncludeRepository := { _ => false },

  libraryDependencies += "eu.cdevreeze.yaidom" %%% "yaidom" % "1.10.0",

  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.1.3",

  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % "test"
)

lazy val root = project.in(file("."))
  .aggregate(introchemistryJVM, introchemistryJS)
  .settings(commonSettings: _*)
  .settings(
    name                 := "introchemistry",
    // Thanks, scala-java-time, for showing us how to prevent any publishing of root level artifacts:
    // No, SBT, we don't want any artifacts for root. No, not even an empty jar.
    publish              := {},
    publishLocal         := {},
    publishArtifact      := false,
    Keys.`package`       := file(""))

lazy val introchemistry = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(commonSettings: _*)
  .jvmSettings(
    libraryDependencies += "com.typesafe" % "config" % "1.3.4"
  )
  .jsSettings(
    // Do we need this jsEnv?
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),

    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6",

    Test / parallelExecution := false
  )

lazy val introchemistryJVM = introchemistry.jvm
lazy val introchemistryJS = introchemistry.js

lazy val pomData =
  <url>https://github.com/dvreeze/introchemistry</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>IntroChemistry is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/introchemistry.git</connection>
    <url>https://github.com/dvreeze/introchemistry.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/introchemistry.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>
