ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "jp.ukiba"

// The default settings
// Defining `ThisBuild / scalaVersion` avoids sbt-updates 0.6.3 to report update for scala-library
ThisBuild / scalaVersion := "3.7.1"  
ThisBuild / scalacOptions ++= Seq(
  "-preview", // https://www.scala-lang.org/api/current/docs/docs/reference/preview/better-fors.html

  "-Xmax-inlines", "64",

  // the default settings from https://scastie.scala-lang.org
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding", "UTF-8",
)

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSModuleKind
import org.scalajs.linker.interface.ModuleKind
inThisBuild(Seq( // apply to every project in the build
  scalaJSLinkerConfig        ~= (_.withModuleKind(ModuleKind.ESModule)),
  Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
))

// use Seq rather than ThisBuild for the default
// https://www.scala-sbt.org/1.x/docs/Multi-Project.html
lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "log4cats-slf4j" % "2.7.1"               % Test, // depends on slf4j-api 1.7.36
    "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.25.1" % Test, // depends on slf4j-api 2.0.17
    "org.slf4j" % "slf4j-api" % "2.0.17"                        % Test, // slf4j-2 is binary compatible with slf4j-1
  ),

  run / fork := true,
  run / baseDirectory := file("."),

  Test / fork := true,
  Test / javaOptions ++= Seq(
    "-Dcats.effect.tracing.buffer.size=8192", // the default 16 would not capture the entire stack trace
  ),
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoOptions ++= Seq(
    BuildInfoOption.BuildTime,
    BuildInfoOption.Traits("jp.ukiba.koinu.build.SbtBuildInfo"),
  )
)

val npmInstall = taskKey[File]("populate node_modules directory")

lazy val ukiba = crossProject(JSPlatform, JVMPlatform).in(file("build/ukiba"))
  .settings(
  ).dependsOn(
    ko_openai,
    ko_aws,
    ko_ffmpeg,
    ko_pdfbox,
    ko_http4s,
    ko_fs2_xml,
    ko_fs2,
    ko_cats_effect,
    ko_html,
    ko_cats,

    ko_scodec_bits,
    ko_java,
    build,
  )

lazy val ko_openai = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_openai"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_openai",

    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-circe" % "1.0.0-M44",
      "io.circe" %%% "circe-generic" % "0.14.14", // json
    ),
  ).dependsOn(ko_http4s, ko_munit % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val ko_aws = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_aws"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_aws",

    libraryDependencies ++= Seq(
      "software.amazon.awssdk" % "auth"     % "2.32.6",
      "software.amazon.awssdk" % "sso"      % "2.32.6", // avoid `To use Sso related properties in the '...' profile, the 'sso' service module must be on the class path.`
      "software.amazon.awssdk" % "ssooidc"  % "2.32.6", // avoid `To use SSO OIDC related properties in the '...' profile, the 'ssooidc' service module must be on the class path.`
      "org.http4s" %%% "http4s-ember-client" % "1.0.0-M44" % Test,
    ),
  )
  .dependsOn(ko_http4s, ko_fs2_xml, ko_munit % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val ko_ffmpeg = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_ffmpeg"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_ffmpeg",

    libraryDependencies ++= Seq(
      // https://github.com/bytedeco/javacpp-presets/tree/master/ffmpeg
      // "org.bytedeco" % "ffmpeg-platform" % "7.1-1.5.11", // for all the platforms, including android and ios
      "org.bytedeco" % "ffmpeg" % "7.1.1-1.5.12", // shared classes
      "org.bytedeco" % "ffmpeg" % "7.1.1-1.5.12"
           classifier "linux-arm64" // armhf for 32bit
           //classifier "linux-x86_64"
           //classifier "windows-x86_64"
           classifier "macosx-arm64",
    ),
  ).dependsOn(ko_fs2)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_pdfbox = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_pdfbox"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_pdfbox",

    libraryDependencies ++= Seq(
      "org.apache.pdfbox" % "pdfbox" % "3.0.5"
        exclude("commons-logging", "commons-logging"),
      "org.slf4j" % "jcl-over-slf4j" % "2.0.17",
    ),
  ).dependsOn(ko_fs2)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_http4s = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_http4s"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_http4s",

    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-dsl"          % "1.0.0-M44",
      "org.http4s" %%% "http4s-client"       % "1.0.0-M44",
      "org.http4s" %%% "http4s-ember-client" % "1.0.0-M44" % Test,
      "org.http4s" %%% "http4s-circe"        % "1.0.0-M44" % Test,
      "io.circe" %%% "circe-generic" % "0.14.14" % Test,
      "io.circe" %%% "circe-parser"  % "0.14.14" % Test,
    ),
  ).jsSettings(
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "fs2-dom" % "0.3.0-M1",
    ),
  ).dependsOn(ko_fs2, ko_munit % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val ko_fs2_xml = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_fs2_xml"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_fs2.xml",

    libraryDependencies ++= Seq(
      "org.gnieh" %%% "fs2-data-xml" % "1.12.0"
        /*
          Avoid the following warning

              [warn] package scala contains object and package with same name: caps.
              [warn] This indicates that there are several versions of the Scala standard library on the classpath.
              [warn] The build should be reconfigured so that only one version of the standard library is on the classpath.
        */
        exclude("org.scala-lang", "scala3-library_3"), // fs2-data-xml 1.12.0 depends on scala3-library_3 3.3.6
    ),
  ).dependsOn(ko_fs2, ko_munit % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val ko_fs2 = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_fs2"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_fs2",

    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-io" % "3.12.0",
    ),
  ).dependsOn(ko_cats_effect, ko_scodec_bits, ko_munit % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val ko_cats_effect = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_cats_effect"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_cats_effect",

    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "3.6.3",
      "org.typelevel" %%% "log4cats-core" % "2.7.1", // there are alternative logging libraries
    ),
  ).dependsOn(ko_cats, ko_munit % "test")
  .enablePlugins(BuildInfoPlugin)

// some classes could be factored out into koinu/ko_munit later
lazy val ko_munit = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_munit"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_munit",

    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1",
      "org.typelevel" %%% "munit-cats-effect" % "2.1.0", // TODO 2.1.0 depends on cats-effect 3.6.0
      "org.typelevel" %%% "log4cats-testing" % "2.7.1",
    ),
  ).dependsOn(ko_cats)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_html = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_html"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_html",

    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % "1.1.0",
    ),
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),
    Test / test := (Test / test)
      .dependsOn(tools / npmInstall)
      .value,

    // scala-js-env-jsdom-nodejs has no support for ES modules yet
    // https://github.com/scala-js/scala-js-env-jsdom-nodejs/issues/56
    Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.NoModule)),
  )
  .jvmSettings(
    Test / skip := true,
  )
  .dependsOn(ko_cats)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_cats = crossProject(JSPlatform, JVMPlatform).in(file("koneko/ko_cats"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koneko.ko_cats",

    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.13.0",
    ),
  ).dependsOn(ko_java)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_scodec_bits = crossProject(JSPlatform, JVMPlatform).in(file("koinu/ko_scodec_bits"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.ko_scodec_bits",

    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.2.4",
    ),
  ).dependsOn(ko_java)
  .enablePlugins(BuildInfoPlugin)

lazy val ko_java = crossProject(JSPlatform, JVMPlatform).in(file("koinu/ko_java"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.ko_java",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.1.1",
    ),
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.1",
      "io.github.cquiroz" %%% "scala-java-time"      % "2.6.0", // java.time implementation
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0", // avoid `ZoneRulesException: Unknown time-zone ID: Asia/Tokyo`
    ),
  ).dependsOn(build)
  .enablePlugins(BuildInfoPlugin)

lazy val build = crossProject(JSPlatform, JVMPlatform).in(file("koinu/build"))
  .settings(
    commonSettings,
    buildInfoSettings,
    buildInfoPackage := "jp.ukiba.koinu.build",
  )
  .enablePlugins(BuildInfoPlugin)

lazy val tools = project.in(file("build/ukiba"))
  .settings(
    npmInstall := {
      implicit val log = streams.value.log
      val dir = file("node_modules")
      if (!dir.exists)
        npm("install", "--silent")
      dir
    },
  )

// Customize sbt
Global / onChangedBuildSource := ReloadOnSourceChanges // reload build.sbt automatically
ThisBuild / sourcesInBase := false // don't include .scala files in the base directory
ThisBuild / turbo         := true  // enable experimental classLoaderLayeringStrategy
