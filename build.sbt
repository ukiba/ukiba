// The default settings of the projects
ThisBuild / scalaVersion := "3.4.2"
ThisBuild / scalacOptions ++= Seq(
  // the default settings from https://scastie.scala-lang.org
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding", "UTF-8",
)

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "jp.ukiba"

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.0.0" % Test,
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.23.1" % Test,
  ),
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoOptions ++= Seq(
    BuildInfoOption.BuildTime,
    BuildInfoOption.Traits("jp.ukiba.koinu.build.SbtBuildInfo"),
  )
)

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .settings(
  ).aggregate(
    ko_http4s,
    ko_fs2,
    ko_cats_effect,
    ko_cats,

    asn1,
    ko_scodec_bits,
    ko_java,
    build,
  )

lazy val ko_http4s = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_http4s"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_http4s",

    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.2.0",
    ),
  ).dependsOn(ko_fs2)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_fs2 = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_fs2"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_fs2",

    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % "3.10.2",
    ),
  ).dependsOn(ko_cats_effect, ko_scodec_bits)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_cats_effect = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_cats_effect"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_cats_effect",

    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "3.5.4",
      "org.typelevel" %%% "log4cats-core" % "2.7.0", // there are alternative logging libraries
    ),
  ).dependsOn(ko_cats)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_cats = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_cats"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_cats",

    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.10.0",
    ),
  ).dependsOn(ko_java)
  .enablePlugins(BuildInfoPlugin)

// TODO probably factor out to a separate repository
lazy val asn1 = crossProject(JSPlatform, JVMPlatform).in(file("koinu/asn1")).in(file("koinu/ko_cats"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.asn1",
  ).dependsOn(ko_scodec_bits)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_scodec_bits = crossProject(JSPlatform, JVMPlatform).in(file("koinu/ko_scodec_bits"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.ko_scodec_bits",

    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.2.0",
    ),
  ).dependsOn(ko_java)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_java = crossProject(JSPlatform, JVMPlatform).in(file("koinu/ko_java"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.ko_java",
  ).dependsOn(build)
  .enablePlugins(BuildInfoPlugin)

lazy val build = crossProject(JSPlatform, JVMPlatform).in(file("koinu/build"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.build",
  )
  .enablePlugins(BuildInfoPlugin)

// Customize sbt
Global / onChangedBuildSource := ReloadOnSourceChanges // reload build.sbt automatically
ThisBuild / sourcesInBase := false // don't include .scala files in the base directory
ThisBuild / turbo         := true  // enable experimental classLoaderLayeringStrategy
