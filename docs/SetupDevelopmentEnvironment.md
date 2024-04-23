# 環境構築

ここでは開発環境の構築手順について解説します。

## Haskellのインストール

[ghcup](https://www.haskell.org/ghcup/)を使うことを強く推奨します。

サイトを開いてでてくるインストール用のシェルスクリプトを実行することで導入することが可能です。

完了して `ghcup` が実行可能になったら次のコマンドを実行してください。

```bash
ghcup install ghc 9.6.4
ghcup install stack 2.15.3
```

これにてUziを開発するためのHaskellの環境構築は完了です。

## DiscordBotの動作確認用のDiscordサーバーのセットアップ

Uziを開発するにあたって、動作確認用のDiscordサーバーを一つ作ってそちらで開発することを強く推奨します。

このセクションでは、実際に動作確認用のDiscordサーバーを作成して、Uziを動かすまでの手順を解説します。

### 1. 自分用のDiscordサーバーを作成
 
1. 画像の箇所をクリックします  
![f80cf14f565429968c01fecc932b4375](https://github.com/himanoa/uzi/assets/18651963/a69239ef-1cb2-4e32-8cb4-4b762ee4ad25)
1. オリジナルを作成をクリックします  
![b27248c49e43b9a7e355ba6a6257f130](https://github.com/himanoa/uzi/assets/18651963/ac1ad9ab-d205-44b5-b76c-3877bc2a7705)
1. 「あなたのサーバーについてもう少し詳しく教えてください」は「自分と友達のため」を選択してください
1. サーバー名と画像は好きに編集してもらって問題ないです。
1. 次の画面になったらサーバーの作成は完了です
![49962f3ae7d2f3170eb6b9330bde816f](https://github.com/himanoa/uzi/assets/18651963/d3c4fd6c-e3e2-401a-a56a-dd414cf4e6c6)

## 2. 動作確認用のBotトークンの取得

デバッグ用のサーバーができたら動作確認用のBotトークンを取得します。このトークンを読み込ませることでUziは初めてDiscordと通信することができます

1. [Discord Developer PortalのApplication](https://discord.com/developers/applications) にアクセスします
2. 「New Application」をクリックします
![0dc1a10a61ae97b9c3ff89a5d3ff5f83](https://github.com/himanoa/uzi/assets/18651963/d457a751-e0f7-40c2-b24b-e842e27881ed)
3. Botの名前を入力し、規約に同意したら 「Create」をクリックします。  
![e90f8e2d94052cfae8d1926e5cc8efd3](https://github.com/himanoa/uzi/assets/18651963/ff44c13e-ec29-42c7-87d5-8ebe22549308)
4. 「Bot」「Token」を参照し、Resetボタンを押すとトークンが出力されるのでこれを控えておきます  
![395b03a7af8ae859079ccc396f24c58d](https://github.com/himanoa/uzi/assets/18651963/af6fa27f-676e-4392-9412-3036763f6925)  
      1. ここで入手したトークンは紛失すると再発行するしかない & Uziを動かすために必要なため、大切に保管しておく必要があります
      2. また、流通した場合他者がこのトークンを利用して、DiscordのAPIを使うことができるので流出しないように気をつけましょう
 
### おすすめのトークン管理方法

macOS or Linuxユーザーの方は uzi のルートディレクトリに `.env` を配置し下記のようなファイル名にして管理しておくことをおすすめします。  
`.env` ファイルな理由は、gitignoreに追加されているため、gitは `.env` ファイルの内容を無視するためトークンが流出しにくいためです。

```
UZI_DISCORD_API_TOKEN="<発行されたトークン>"
```

この方式で管理しておくことで、Uziの開発をする時のみ `source .env` するとAPIトークンが読み込まれ、`stack run` するだけでUziを万全の状態で起動することができます。

### 3. 動作確認用のDiscordBotを自分のサーバーに追加する

最後に先ほどのセクションで作った動作確認用のDiscord Applicationを自分が作ったサーバーに参加させましょう。  
このセクションではBotが必要な権限設定と、OAuthLinkを用いて自分の作ったサーバーにBotを参加させるまでの解説を行います。

1.  [Discord Developer PortalのApplication](https://discord.com/developers/applications) から 「Bot」を開き 「Privileged Gateway Intents」の 「Message Content Intent」を有効にします
![78741e2f9050210f8a32fbc1b56e66f0](https://github.com/himanoa/uzi/assets/18651963/2789a8b9-84c1-46c6-94d6-5e20452b49d8)
2. その後出てきたポップアップの 「Save Changes」ボタンをクリックして設定を有効化します。
3. 「OAuth2」をクリックして、「OAuth2 URL Generator」の中にある「bot」をクリックしてチェックを入れます
![26e682a31fb3e2c59961b9cc2635e8f8](https://github.com/himanoa/uzi/assets/18651963/88c67947-ca5a-4f6d-94b0-42c5ca9990c2)
4. 3を実施すると、下部に大量のチェックマークが入った表示になるので「Manage Server」「Manage Channels」にチェックを入れて、生成されたURLを踏みます
![6e27f84d60f58e97742ccffd3aaf84b0](https://github.com/himanoa/uzi/assets/18651963/389183d2-54b1-4bd4-8b2f-773bc8ddf759)
5. 下記の画面になったら、追加するサーバーに先ほど作ったサーバーを選択し、「はい」をクリックしてDiscord Applicationをサーバーに参加させます  
![f110d679070e402d79c7382ec2a3323e](https://github.com/himanoa/uzi/assets/18651963/91b9c752-ec91-4eed-845a-5db62532c59c)
7. 自分のDiscordの #一般 チャンネルにBotが参加してきたログが表示されたら完了です。お疲れさまでした。  
  ![image](https://github.com/himanoa/uzi/assets/18651963/ff91cda0-51a2-45d9-b00a-77f40cb24627)





