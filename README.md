This is an amalgamation of my code that could be reusable.

# The directory layout

|Name                   |Role|
|-----------------------|----|
|`jvm/`                 |Programs for a Java virtual machine|
|`js/`                  |Programs for a JavaScript runtime, including the browser|
|`shared/`              |Programs that do not depend on the environment|
|`koneko/`              |Libraries that depend on [cats](https://github.com/typelevel/cats)|
|`koinu/`               |[Scala](https://www.scala-lang.org/) libraries|
|`build.sbt`, `project/`|The build scripts|


# The envoronments

## Build environment

1. Java 21 and [sbt](https://www.scala-sbt.org/)

    1. [Coursier](https://get-coursier.io/) and
       [Corretto](https://aws.amazon.com/corretto/)
       can be used to setup a shell, for example

           # set JAVA_HOME and PATH
           # optionally downloading the Java version if not already installed
           eval $(cs java --jvm corretto:21 --env)


# The builds

Launch `sbt`, then

1. Build the jar files

       packageBin

## Tests

1. Run the unit tests (both on Java and JavaScript)

       test
