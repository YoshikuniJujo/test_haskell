TODO
====

* [x] ブラウザの設定画面の組み込みの設定画面にはタブ指定でメッセージを送れないことを確認する
	+ [x] 設定画面からbackgroundにメッセージを送りそこからtab IDを取得する
	+ [x] background.jsからそのタブIDにメッセージを送りエラーが出ることを確認する
* [x] openTypeとpageIdを読み込む(両方ともないときはbrowserにする)
* [x] ブラウザの設定画面の組み込みの設定画面にはタブ指定でポートを接続できないことを確認する
	+ [x] 今はoptions.jsの立ち上げと同時に送られているメッセージをボタンを押して送るようにする
+ [x] メッセージについてテストの流れをチェック
	+ [x] 流れを読む
	+ [x] コードを修正しリファクタリングする
+ [x] ポートについてテストの流れをチェック
* [x] 上記のチェックで使ったコードを整理する
* [x] background.jsでopenOptionsTabを関数にする
* [x] 設定画面からbackgroundに対してportを接続する
	+ [x] まずは単純に
	+ [x] できたらonConnectへのリスナーを一時的なものにする
* [x] 上記のポートでやりとりを試す
* [x] 設定画面からbackgroundへの接続からのやりとりのコードを整理する
	+ [x] 流れを読む
	+ [x] コードを修正しリファクタリングする
* [x] backgroundから設定画面に対してportを接続する
	+ [x] 設定画面からbackgroundにメッセージを送りそこからtab IDを取得する
	+ [x] portを接続する
* [x] 上記のポートでやりとりを試す
* [x] ensurablePort.jsをcopyする
* [x] クラスEnsurablePortを定義する
* [x] EnsurablePortを使ってみる
	+ [x] options.htmlにTest Ensurable Portボタンを置く
	+ [x] options.jsからメッセージを送る
	+ [x] background.jsはlistenerを設定する
	+ [x] options.jsからensureする
	+ [x] 上で取り出したportにmessageを送る
	+ [x] background.jsでmessageを受け取る
* [ ] 後始末をする
	+ [x] background.jsでlistenerを消す
	+ [ ] EnsurablePortにdisposeを定義する
* [ ] EnsurablePortのリファクタリング
* [ ] EnsurablePortListを使ってみる
* [ ] クラスEnsurablePortListを実装する
	+ [ ] sendを追加するなど
* [ ] EnsurablePortListのリファクタリング
