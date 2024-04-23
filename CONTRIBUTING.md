# コントリビュートするためには

## Issues

次のIssueを受け付けています。

1. バグ報告
1. 新しい機能のアイデア
1. 実装の改善

その他のIssueも歓迎しています。

## Pull-Request

Pull-Requestはいつでも歓迎しています。基本的にIssueを立てずにPull-Requestを送ってもらって問題ありません。

次のPull-Requestは特に歓迎します。  
ここに書いていないものでも、Issueを立てたり直接メンテナに相談してください。  

- テスト追加
- ドキュメントの修正
- バグ修正
- 機能追加
  - 基本的には受け入れるようにしますが、危険な機能な場合はメンテナ判断で拒否される可能性もあります
- ランタイムの更新など


### 受け入れられないPull-Request

メンテナがこのドキュメントを読んでないと判断したPull-Requestの場合

## パッチの送り方

次のワークフローに基づいてパッチを送ってください  
環境構築を行なっていない場合 TODO: 

1. Forkする
1. `git clone <forkしたあなたのリポジトリ>`
1. フォーマットなどに必要なツールのインストール `bin/install-deps`
1. Branchを作成する `git checkout -b <feature-name>`
1. テストが通るか確認する `stack test`
1. コードを変更する
1. フォーマッターを実行する `bin/format`
1. 変更をコミットする `git commit -am <commit message>`
1. 変更を適用した状態でテストが通るか確認する `stack test`
1. 変更をpushする `git push origin HEAD`
1. Pull-Requestを送る

## コミットメッセージ

現在は英語で書かれていますが、日本語で書いても問題ありません。  
**本リポジトリでは [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) は採用していません そのため、Convertional Commitsの流儀のタグが付いたコミットメッセージを書かないでください。**  
また、コミットメッセージは変更の意図はなるべく明瞭なものが好ましいです。

### 例

:x: Bad: `fix: foobar bug`  
:o: Good: `helpコマンドが末尾にスペースが入っていた場合に動かない問題の修正`

## 困ったときは？

作業を進めていて困った場合はカジュアルにメンテナに相談してください。

該当するIssueがある場合はIssue上で @himanoa にmentionしていただくか、Discord上でコミュニケーションを取ってもらえると助かります。

作業が止まってパッチが送信されないことが一番の損失なため、どんなことでも相談してもらえると嬉しいです。
