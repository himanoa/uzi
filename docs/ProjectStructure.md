## プロジェクト構成

下記のような形になっています。

詳細なドキュメントは https://himanoa.github.io/uzi/uzi-0.2.2.0 を参照してください

```bash
$ tree -I dist-newstyle
.
├── app アプリケーションのエントリーポイントです。基本的にここにはコードを書かずに、src配下にコードを書いていきます
├── bin 開発するために便利なコマンドをシェルスクリプトで書いているディレクトリです。
│   ├── format
│   └── install-deps
├── cabal.config
├── cabal.project
├── cabal.project.freeze
├── CHANGELOG.md
├── CONTRIBUTING.md
├── docs 各種ドキュメント
│   ├── HELP.md
│   ├── ProjectStructure.md
│   ├── Release.md
│   └── SetupDevelopmentEnvironment.md
├── Earthfile
├── LICENSE
├── output
│   └── uzi-exe
├── package.yaml
├── README.md
├── src ソースコードです。Uziでやる処理は全てここに書いてあります
│   ├── Data データ構造やそれに紐付いた関数を定義しています
│   │   ├── Discord Discord由来のデータ構造や関数を扱うモジュールです
│   │   │   ├── Request
│   │   │   ├── Response
│   │   ├── Uzi  宇治共和国特有のルールなどをモデリングした構造や処理が書かれているモジュールです
│   ├── Effectful ログ出しやHTTPなど IO が絡んだり、純粋関数でどうしても書けないAPIを置いている場所です。
│   │   ├── BotUser
│   │   ├── DiscordApiTokenReader
│   │   ├── DiscordApplication
│   │   ├── DiscordChannel
│   │   ├── DiscordGateway
│   │   ├── DiscordSlash
│   │   ├── DynamicLogger
│   │   ├── Http
│   ├── EventHandler Discordのイベントに対して発生する処理についてまとめたモジュールです
│   │   ├── MessageCreateEventHandler MessageCreateイベント(サーバーで発生したメッセージの送信)に反応するハンドラをまとめたモジュールです。
├── stack.yaml
├── stack.yaml.lock
├── test テストです
│   ├── Data
│   │   ├── Discord
│   │   │   └── Response
│   │   └── Uzi
│   ├── Effectful
│   │   ├── BotUser
│   │   └── DiscordChannel
│   ├── EventHandler
│   │   └── MessageCreateEventHandler
│   ├── Helper
└── uzi.cabal
```
