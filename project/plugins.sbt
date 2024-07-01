// cross compile for JVM & JS
// TODO: cross compile for native too: like https://github.com/typelevel/log4cats/blob/main/project/plugins.sbt
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.16.0")

// generate the version and timestamp of the build
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.12.0")

// dependencyUpdates
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")
