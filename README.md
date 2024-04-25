# uzi
[![ci](https://github.com/himanoa/uzi/actions/workflows/ci.yaml/badge.svg)](https://github.com/himanoa/uzi/actions/workflows/ci.yaml)

宇治共和国用のDiscordBotです

## 機能

See. https://github.com/himanoa/uzi/blob/master/docs/HELP.md

## アプリケーションが使う環境変数

- `UZI_DISCORD_API_TOKEN`: DiscordのGatewayAPI用のAPIトークンです(必須)
- `UZI_IS_DEBUG`: 1 or 0 Debug用のオプションです。DiscordAPIのトレースが標準出力に流れるようになります

## Botを動かす

[環境構築](./docs/SetupDevelopmentEnvironment.md)を完了させてください。
その後、次のコマンドを実行するとUziを起動することができます。

```bash
`UZI_DISCORD_API_TOKEN=<your token> stack run`
```

`UZI_DISCORD_API_TOKEN` は環境変数に既に入っている場合は、省略可能です

## テストの実行

次のコマンドを実行することでテストを実行することができます。 `stack test`

## ドキュメントのビルド

次のコマンドを実行してください

```bash
stack build --haddoc
```

## コントリビューション

See. [CONTRIBUTING.md](./CONTRIBUTING.md)

