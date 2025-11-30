A collection of my code that could be reusable.

# The directory layout

|Name                   |Role|
|-----------------------|----|
|`jvm/`                 |Implementations for a Java virtual machine|
|`js/`                  |Implementations for a JavaScript runtime, including the browser|
|`shared/`              |API and Implementations that do not depend on the runtime|
|`koneko/`              |Libraries that depend on [cats](https://github.com/typelevel/cats)|
|`koinu/`               |[Scala](https://www.scala-lang.org/) libraries|
|`build.sbt`, `project/`|The build scripts|


# The envoronments

## Build environment

1. Java 25 LTS and [sbt](https://www.scala-sbt.org/)

    1. With [Coursier](https://get-coursier.io/),
       a shell environment can be setup with, for example, [Corretto](https://aws.amazon.com/corretto/)

           # set JAVA_HOME and PATH
           # optionally downloading the Java version if not already installed
           eval $(cs java --jvm corretto:25 --env)

1. Node.js 24 LTS

    1. Mac: To install with [Homebrew](https://brew.sh/)

           brew install node@24
           brew link --overwrite --force node@24

        1. This will make the LTS version the default,
           even if the current version of node (v25) is also installed.

            1. When the current version had been installed,
               those hard wired to the current version still use it, since

                   /opt/homebrew/bin/node           # node v24 (default in PATH)
                   /opt/homebrew/opt/node/bin/node  # node v25 (always the unversioned one if exists)

                1. For example, the next would show if emscripten still uses the current version

                       EMCC_DEBUG=1 emcc 2>&1 | grep -i node


# The builds

Launch `sbt`, then

1. Build the jar files

       packageBin

## Tests

1. Run the unit tests (both on Java and JavaScript)

       test
