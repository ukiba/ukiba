This is an amalgamation of my code that could be reusable.

# The directory layout

|Name                   |Role|
|-----------------------|----|
|`jvm/`                 |Programs for a Java virtual machine|
|`js/`                  |Programs for a JavaScript runtime, including the browser|
|`shared/`              |Programs that do not depend on the runtime|
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

    1. Mac: With [Homebrew](https://brew.sh/)

           brew install node@24

           # If the current version of node (v25) is also installed,
           # this will make the LTS version the default
           brew unlink node
           brew link --overwrite node@24

           # after this, those hard wired to the current version still get it
           #   /opt/homebrew/bin/node           # node v24 (default in PATH)
           #   /opt/homebrew/opt/node/bin/node  # node v25
           # The next command would show if emscripten still uses the hard wired version
           #   EMCC_DEBUG=1 emcc 2>&1 | grep -i node


# The builds

Launch `sbt`, then

1. Build the jar files

       packageBin

## Tests

1. Run the unit tests (both on Java and JavaScript)

       test
