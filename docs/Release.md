## リリースフロー

主に@himanoaが行う作業です

1. 最新のmasterをpull `git pull origin master`
1. package.yamlとEarthlyのバージョン番号を変更
1. `earthly +docker` を実行
1. `docker image push himanoa/uzi:{version}` を実行

