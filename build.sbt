// If using Scala.js 0.6.x, also add the following import:
//import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion := "2.13.1"

lazy val root = project.in(file(".")).
  aggregate(wuji.js, wuji.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val wuji = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "wuji",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
  )