// Scala.js
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

// generate the version and timestamp of the build
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

// dependencyUpdates task to list dependencies that have updates
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")
