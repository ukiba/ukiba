ukiba [Scala](https://www.scala-lang.org/) libraries.

# The project layout

|Name                   |Role|
|-----------------------|----|
|`koneko/`              |Functional programming libraries that depend on [cats](https://github.com/typelevel/cats)|
|`koinu/`               |Scala libraries that do not depend on any of the above|
|`build.sbt`, `project/`|The build scripts|


# The envoronments

## Build environment

1. Java 21 and [sbt](https://www.scala-sbt.org/)

    1. For example with [Coursier](https://get-coursier.io/) and [Corretto](https://aws.amazon.com/corretto/)

           # set JAVA_HOME and PATH, optionally downloading the Java version when not available
           eval $(cs java --jvm corretto:21 --env)


# The builds

Launch `sbt`, then

1. Build the jar files

       packageBin

## Tests

1. Run the unit tests (both on Java and JavaScript)

       test
