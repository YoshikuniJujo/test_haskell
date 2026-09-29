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
* [x] 後始末をする(途中)
	+ [x] background.jsでlistenerを消す
* [x] EnsurablePortやEnsurablePortListの「名前」を定義する
	+ [x] base nameを定義する
	+ [x] base nameと指定された名前を結合してインスタンス内部の#nameにする
* [x] 後始末をする(続き)
	+ [x] EnsurablePortにdisposeを定義する
* [x] EnsurablePortListを使ってみる
	+ [x] portを接続する
	+ [x] メッセージを送信する
* [x] TypeScriptで拡張機能を書くモデルを作る
* [x] EnsurablePortクラスのインスタンスの作成をリスナー内にする
* [x] EnsurablePortListクラスのインスタンスの作成をリスナー内にする
* [x] クラスEnsurablePortListを修正する(途中)
	+ [x] disconnectを実装する
		- [x] options.jsからdisconnectメッセージを送信する
		- [x] #listenerを定義する
		- [x] EnsurablePortListの#disconnectをtrueにする
		- [x] EnsurablePortListからACKを送る
		- [x] options.jsでdisconnectする
		- [x] disconnectを検出して#disconnectがtrueなら#disposeする
* [x] EnsurablePortにdisconnectメソッドを追加
	+ [x] options.htmlにTest EnsurablePort reverseボタンを追加
	+ [x] Test Ensurable Portと同様にするがメッセージを逆方向にする
	+ [x] EnsurablePortにdisconnectメソッド(空)を追加
	+ [x] EnsurablePort側からdisconnectメッセージを送信
	+ [x] EnsurablePort側で#listenerをremoveする
	+ [x] EnsurablePort側で#disconnectフラグをtrueにする
	+ [x] background側からACKを送信
	+ [x] EnsurablePort側でACKを受信
	+ [x] EnsurablePort側からdisconnect
	+ [x] background側でlistenerをremoveする
* [x] クラスEnsurablePortListを修正する(続き)
	+ [x] postを追加するなど
		- [x] postを定義
		- [x] #postWithTabを定義
		- [x] #postWithoutTabを定義
		- [x] portの接続を受けたらwithoutのほうでためたキューのメッセージを注ぎ込むようにする
* [ ] EnsurablePortListにdisconnectメソッドを追加
* [ ] EnsurablePortのリファクタリング
* [ ] EnsurablePortListのリファクタリング
