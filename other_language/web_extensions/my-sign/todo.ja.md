TODO
====

* [x] クライアントの編集
	+ [x] 一覧表示
	+ [x] 選択、詳細表示
	+ [x] データベースへの修正
* [x] nameのところにundefinedという文字列が表示されることについて
* [x] 新規クライアントの作成を「編集」のほうに統一する
	+ [x] 新しい「新規作成」を追加
	+ [x] 古い「新規作成」を削除
* [x] クライアント詳細画面に「キャンセル」ボタンを追加
* [x] ApplyをOKに修正する [Cancel] [OK]
* [x] クライアントの削除
	+ [x] [Delete]の設置
	+ [x] 中身を書く
	+ [x] 新規作成の場合にはhiddenにする
* [x] 実機でテスト
* [x] scriptsにrun-keep.jsを追加
* [x] URLフラグメントhiddenにする
* [x] クライアントのマッチの優先順位
	+ [x] HTMLに入力用のフォームを用意する
	+ [x] 関数loadClientToFormを定義する
	+ [x] 編集でpriorityをいじれるようにする
		- [x] データベースからフォームへの読み込み
		- [x] フォームからデータベースへの書き込み
	+ [x] マッチのところを修正
* [x] 画面へのアカウント表示
	+ [x] クライアントのURLからアカウントを取得するところを関数にする
	+ [x] EncryptedSecretKeyクラスにnameのgetterを作る
	+ [x] 公開鍵からEncryptedSecretKeyクラスのインスタンスを取り出す関数を作成する
	+ [x] get-accountがnullまたはアカウントを返すようにする
	+ [x] URLがクライアントにマッチしている場合のみ表示とする
	+ [x] 今ダミーとしている表示を「アカウント名 + 公開鍵の先頭15文字ほど」にする
	+ [x] アカウントの変化があったら表示も変化させるようにする
		- [x] options.jsからbackground.jsにメッセージを送信する
		- [x] background.jsから全てのタブにメッセージを送信する
		- [x] content.jsではメッセージを受け取り表示領域の初期化の処理を走らせる
	+ [x] アカウント表示の有無を示す値をクライアントに追加
	+ [x] アカウント表示の位置を示す値をクライアントに追加
		- [x] 設定画面に追加
		- [x] データベースからフォームへ
		- [x] フォームからデータベースへ(submit)
		- [x] get-accountの返答に追加
		- [x] content.jsでの表示位置に反映
	+ [x] デフォルト値は「右上」に「表示」としておく
	+ [x] アカウント表示部分の色と透過度を選べるようにする
		- [x] 設定画面に追加
		- [x] データベースからフォームへ
		- [x] フォームからデータベースへ(submit)
		- [x] 色の表現の変換関数を定義する
		- [x] get-accountの返録に追加
		- [x] content.jsの色と透過度に追加
* [x] URLフラグメントのクライアント選択へのオプション追加はなしにする
	+ [x] 設定画面に追加
	+ [x] データベースからフォームへ
	+ [x] フォームからデータベースへ(submit)
	+ [x] URLフラグメント用に用意したものを消す
* [x] パスワード入力欄にアカウント名とnpubの最初の25文字くらいを表示
* [x] ユーザーがタブを閉じた時の動作を実装
* [x] タブ内で他のページに移動したときの処理を実装
* [x] 未登録クライアントから誘導される設定画面タブの管理(途中)
	+ [x] Promise.withResolversでresolveとrejectをMapに保存
	+ [x] タブを開いて設定用ページに接続するようにする
	+ [x] new clientボタンを押しておく
	+ [x] new clientボタンを押したときの処理を関数化する
	+ [x] URLPatternのところをうめておく
	+ [x] InputTabs.assign
* [x] backgroundのリスナーの中のべた書きを関数にする
* [x] 使用者、役割、機能をmemo.ja.mdにまとめる
	+ [x] get-account
	+ [x] get-public-key
	+ [x] queryPswd
	+ [x] returnPswd
	+ [x] contentStarted
	+ [x] sign-event
	+ [x] clientChanged
* [x] 7つのAPIについて名前を変更する(途中)
	+ [x] getAccount -> accountDisplayInfo
	+ [x] getPublicKey -> publicKey
* [x] もう1台のPCでもrun-keepでフラグメントによるアカウントが変更できるような設定を保存する
	+ [x] アカウントは3つくらい用意しよう
* [x] 7つのAPIについて名前を変更する(続き)
	+ [x] queryPswd -> prepareSymmetricKey
	+ [x] returnPswd -> registerSymmetricKey
	+ [x] wrongPswdを送る代わりにregisterSymmetricKeyの返り値とする
	+ [x] signEvent
	+ [x] contentStarted
	+ [x] clntChanged -> clientChanged
+ [x] 表示領域の背景の色を#rrggbbで指定できるようにする
+ [ ] ブラウザの「拡張機能の設定」から開いた設定画面をInputTabsの管理下に置く
+ [ ] optionsStateのaccountの部分を書く
+ [ ] optionsStateのaccountの部分を実際に使ってみる
+ [ ] options.jsの仕様を考える
* [ ] options.jsから直接データベース処理をするのではなくbackgroundに依頼するようにする
	+ [ ] 設定画面の「状態」を表すオブジェクトを設計する
		- [ ] memo.ja.md上で
		- [ ] コード上で
	+ [ ] storage.session上の状態をinputなどで変化させる
	+ [ ] storage.session上の状態から復元する
	+ [ ] OKのときstorage.session上の状態からデータベースに書き込む
	+ [ ] Cancelではstorage.session上の状態を削除する
	+ [ ] Deleteではデータベース上からエントリーを削除する
	+ [ ] パスワード入力時の処理を記述
	+ [ ] options.jsからデータベースをいじる処理を削除する
	+ [ ] その他
* [ ] 未登録クライアントから誘導される設定画面タブの管理(続き)
	+ [ ] InputTabs.complete
	+ [ ] InputTabs.tabClosed
	+ [ ] 仕様を検討する
	+ [ ] テスト
	+ [ ] 残り
* [ ] パスワード入力用タブの異常系のテストのために/others/try-sign.htmlを修正する
	+ [ ] ボタンを押すとイベントに署名するようにする
	+ [ ] コンテンツ部分を入力できるようにする
* [ ] パスワード入力用タブの異常系
	+ [ ] 同じタブから3回パスワードを送る
	+ [ ] 異なるタブから3つのアカウントを開く
* [ ] アカウント表示領域のクリックを設定可能にする
* [ ] クライアントの権限テーブル
* [ ] ncryptsecのエクスポート
* [ ] ncryptsecのインポート
* [ ] nsecのインポート
* [ ] editingClientではなくeditingClientUuidとする
	+ 編集のときクライアント全体ではなく、そのUUIDだけモジュールスコープの変数に保持すればいい
* [ ] getPublicKeysWithNamesの再検討、必ずEncryptedSecretKeyインスタンスを介するようにする
* [ ] フォームの内容をデータベースに書き込む時の検証について考える
	+ とくにBech32.decode()のところ
	+ 今のところHTML側でvalidateしてJS側では例外を発生させるので良いと考えている
