TODO
====

TODO
----

* [x] リファクタリング
	+ [x] content.js
	+ [x] background.js
	+ [x] input.js
	+ [x] input.html
* [x] 確認
	+ [x] mutex.js
	+ [x] inputTab.js
	+ [x] manifest.json
* [ ] タブを閉じたのを検出するのではなくportの切断を検出する
	+ [x] 入力用タブ側からportをつなぐ
	+ [x] background側で接続、切断を検出
	+ [x] 何かを互いに送ってみる
	+ [ ] background.jsのはじめで新しいportでつなぎ直す
	+ [ ] portの切断を検出する
	+ [ ] portの切断の検出をタブを閉じたことの検出の代やりに使う
* [ ] メッセージの送受信をportで行うようにする
* [ ] テスト
	+ [ ] npm run start
	+ [ ] bidiによるテスト
	+ [ ] npm run start-android

DIRECTORY
---------

```
project/
  +-- src/
  |    +-- content.js
  |    +-- background.js
  |    +-- manifest.json
  |    +-- input/
  |    |      +-- input.html
  |    |      +-- input.js
  |    |      +-- inputTabs.js
  |    |      +-- mutex.js
  |    |
  |    +-- codec/
  |    +-- crypto/
  |    +-- generated/
  |
  +-- tools/
  |
  +-- ...
```
