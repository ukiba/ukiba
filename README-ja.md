再利用できそうなコードの寄せ集め。

# ディレクトリ構成

|名称                   |役割|
|-----------------------|----|
|`jvm/`                 |Java 実行環境用のプログラム|
|`js/`                  |JavaScript 実行環境用のプログラム|
|`shared/`              |実行環境に依存しないプログラム|
|`koneko/`              |[cats](https://github.com/typelevel/cats) に依存するライブラリ|
|`koinu/`               |[Scala](https://www.scala-lang.org/) ライブラリ|
|`build.sbt`, `project/`|ビルド用スクリプト|


# 環境

## ビルド環境

1. Java 25 と [sbt](https://www.scala-sbt.org/) ビルドツール

    1. [Coursier](https://get-coursier.io/) と
       [Corretto](https://aws.amazon.com/corretto/) で
       シェル環境を準備する例

           eval $(cs java --jvm corretto:25 --env)

        1. 上記コマンドの実行前に Coursier をセットアップする必要があります
            1. Coursier をセットアップすると `java` と `sbt` もインストールされます
            1. [Windows で Coursier をセットアップ](https://get-coursier.io/docs/cli-installation#windows) するには

                   curl -OL --fail https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-win32.zip
                   tar xf cs-x86_64-pc-win32.zip
                   cs-x86_64-pc-win32.exe setup

                   Should we add ~\AppData\Local\Coursier\data\bin to your PATH? [Y/n] Y

                1. インストール後、環境変数を反映させるために一度サインオフ (または再起動) する必要があります


# ビルド手順

`sbt` を起動してから

1. jar ファイルを構築

       packageBin

## テスト

1. 単体テストを実行 (Java と JavaScript 両方)

       test
