ukiba の [Scala](https://www.scala-lang.org/) ライブラリ群。

# プロジェクト構成

|名称                   |役割|
|-----------------------|----|
|`koneko/`              |[cats](https://github.com/typelevel/cats) に依存する関数型ライブラリ|
|`koinu/`               |上記に依存しない Scala ライブラリ|
|`build.sbt`, `project/`|ビルド用スクリプト|


# 環境

## ビルド環境

1. Java 21 実行環境と [sbt](https://www.scala-sbt.org/) ビルドツール

    1. コマンドプロンプトから [Coursier](https://get-coursier.io/) と [Corretto](https://aws.amazon.com/corretto/) を使う場合は

           eval $(cs java --jvm corretto:21 --env)

        1. 上記コマンドの実行前に Coursier をセットアップする必要があります
            1. java と sbt もインストールされます
            1. [Windows で Coursier をセットアップ](https://get-coursier.io/docs/cli-installation#windows) するには

                   curl -OL --fail https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-win32.zip
                   tar xf cs-x86_64-pc-win32.zip
                   cs-x86_64-pc-win32 setup

                   Should we add ~\AppData\Local\Coursier\data\bin to your PATH? [Y/n] Y

                1. インストール後、環境変数を反映させるために一度サインオフ (または再起動) する必要があります


# ビルド手順

`sbt` を起動してから

1. jar ファイルを構築

       packageBin

## テスト

1. 単体テストを実行 (Java と JavaScript 両方で)

       test
