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
* [x] 表示領域の背景の色を#rrggbbで指定できるようにする
* [x] ブラウザの「拡張機能の設定」から開いた設定画面をInputTabsの管理下に置く
	+ [x] 置かない置かない
* [x] Shadow DOMは使わない
* [x] アカウント表示領域のクリックを設定可能にする(途中)
	+ [x] 設定の項目に追加
		- [x] HTMLに追加
		- [x] .jsでデータベースからの読み込み
		- [x] .jsでデータベースへの書き込み
	+ [x] 透過させない場合のリンクを設定
* [x] optionsStartedあたりを消していく
* [x] ブラウザの「拡張機能の設定」から開いた設定画面のあつかいを考える
* [x] backgroundの受け取るメッセージをメソッドとして解釈するようにする(途中)
	+ [x] 今あるメソッドについてはclassやinstanceがundefinedの場合とする
	+ [x] classがundefinedのときにはそれ用の関数を呼ぶ
* [x] optionsStateのaccountの部分を書く
	+ [x] アカウント名の入力中にインスタンス側に写しを作る
	+ [x] アカウント名の入力中にstorage.sessionに写しを作る
	+ [x] writeを実装する
* [x] OptionsStateクラスのインスタンスにidを追加
* [x] ログ表示機能を設定画面に追加する(途中)
	+ [x] ログ用のデータベースを用意する
	+ [x] ログの書き込み関数を定義する
	+ [x] ログの読み出し関数を定義する
	+ [x] ログ表示領域を用意する
	+ [x] ログを書き出す
* [x] 4番目の設定画面の開きかたを実装する
	+ [x] タブで開くボタンを追加
	+ [x] タブで開くボタンからbackgroundにメッセージを送る
	+ [x] 設定画面を開く
* [x] 4個の設定画面の開き方について全部InputTabs.assign()する
	+ [x] options.jsからbackgroundにメッセージ
		- "browser"と"tab"と"UUID"
		- つまり"URL"以外
		- optionsStartedあたりか
	+ [x] クエリを修正するUUIDとURL
	+ [x] ブラウザから
	+ [x] タブで開く
	+ [x] アカウント表示領域から
	+ [x] getPublicKeyから
* [x] options.jsでクエリを読み出す
	+ [x] クエリを付ける部分を修正する
	+ [x] クエリを読み出す
	+ [x] console.logやLog.writeで確認する
* [x] URLのerror recoveryでerrorのときにエラーメッセージを表示する
	+ [x] clientSubmitedの返り値を表示
	+ [x] エラー表示領域を作成
	+ [x] エラーを表示する
* [x] 設定画面のotbsについてタグを閉じたのに対応する
	+ [x] pgVanishedにotbsの処理を追加する
	+ [x] URLの「元タブ」に対してイベントを送る
	+ [x] content.jsではイベントを受け取りejectする
	+ [x] 元タブが閉じた時にはそのURLで開いた設定画面も閉じる
* [x] InputTabsから入力用タブのIDを全部取り出せるようにする
* [x] パスワード入力画面を閉じたときに元タブの1番目にもどるようにする
* [x] 設定画面のタブ管理について確認する(1)
	+ [x] getPublicKeyのリカバリのための設定画面が閉じたときの動きを確認
		- 元タブをactiveにする
* [x] パスワード入力かキャンセルされたときに元ページにエラーメッセージを出す
* [x] エラーメッセージを中央にする
* [x] UUIDタブを閉じたら元の画面にもどる
	+ [x] キーがURLかUUIDかを判別できるようにする
	+ [x] キーがURLのときだけforEach...をするようにする
* [x] 設定画面のタブ管理について確認する(2)
	+ [x] アカウント表示画面から開いた設定画面(set不使用時)
		- [x] UUIDでopenした場合にも元タブを追加する
	+ [x] アカウント表示画面から開いた設定画面(set使用時)
		- [x] 設定画面にSetを使用するチェックボックス
		- [x] unit型を定義する
		- [x] unit型をキーにしたuseClientSetテーブルを作成する
		- [x] データベースからformへの読み出し
		- [x] formからデータベースへの書き込み
		- [x] UUIDの代わりにuse setが設定されていたら"set"にする
		- [x] 全体でひとつにする
		- [x] タブを閉じたら元の画面にもどる
	+ [x] ブラウザの設定画面
	+ [x] タブとして開いた設定画面
	+ [x] getPublicKeyのリカバリのための設定画面
* [x] InputTabsから[(key, value)]を取り出せるようにする
* [x] InputTabsから取り出した[(key, values)]の確認
	+ [x] browser側から開いた設定画面
	+ [x] 「タブとして開く」から開いた設定画面
	+ [x] getPublicKeyのリカバリから開いた設定画面
	+ [x] 「アカウント表示領域」から開いた設定画面(set不使用時)
	+ [x] 「アカウント表示領域」から開いた設定画面(set使用時)
* [x] InputTabsから取り出した[(key, values)]の確認(実機)
	+ [x] browser側から開いた設定画面
	+ [x] 「タブとして開く」から開いた設定画面
	+ [x] getPublicKeyのリカバリから開いた設定画面
	+ [x] 「アカウント表示領域」から開いた設定画面(set不使用時)
	+ [x] 「アカウント表示領域」から開いた設定画面(set使用時)
* [x] InputTabsから返すのはkeyだけでいいかもしれない(そうじゃないかもしれない)
	+ tab idはOptionsStateに保存する
	+ むしろtab idはInputTabs側に保存するべきかもしれない
* [x] InputTabs.inputTabsは消す
	+ [x] InputTabs.keyInputTabsを使うようにする
	+ InputTabs.keyInputTabsを残す
* [x] ログ出力時に設定画面にメッセージを送るようにする
	+ [x] logをクロージャにする
	+ [x] ログ出力用タブの設定追加機能をつける
	+ [x] options.jsから「ログ出力タブへの登録」メッセージを送る
	+ [x] 上記メッセージを受け取りログにタブを追加
	+ [x] optionsタブが閉じたときにログ出力タブを削除する
		- もしかすると、何かしらのタブを閉じたときに全体を更新するのがはやいかも
		- その場合は「削除」機能は必要なくて、初期化機能が必要になる
	+ [x] ログ出力用タブにメッセージを送るようにする
	+ [x] optionsタブでメッセージを受け取り、ログを更新するようにする
* [x] 設定画面はメッセージを受けてログ出力を更新する
* [x] logUpdatedの送信をconnect/portを利用するようにする(途中)
	+ [x] background.jsとoptions.jsにensurePortを定義する
	+ [x] onConnectを定義
		- [x] options.js
		- [x] background.js
	+ [x] logUpdatedの送信に使ってみる(途中)
		- [x] keyInputTabsの返り値をオブジェクトからMapにする
		- [x] log.writeにはidがbrowser以外のtab idを引数としてわたす
* [x] EnsurablePortクラスを定義(中断)
	+ [x] クラスを定義
	+ [x] options.jsで使ってみる(やめた)
* [x] EnsurablePortListクラスを定義(中断)
	+ [x] クラスを実装
	+ [x] background.jsで使ってみる(やめた)
* [x] logUpdatedの送信をconnect/portを利用するようにする(中断)
	+ [x] logUpdatedの送信に使ってみる(中断)
		- [x] log.writeにportsを渡すことを検討する(やめた)
		- [x] log.writeのときにensurePortをしてlogUpdatedを送る(やめた)
		- [x] focusのときにensurePortをする(やめた)
* [x] TypeScriptに移行する
	+ [x] npm install --save-dev typescript @types/firefox-webext-browser
	+ [x] npx tsc --init
	+ [x] codec/word.ts
	+ [x] codec/polymod.ts
	+ [x] codec/bech32.ts
	+ [x] background.ts
	+ [x] content.ts
	+ [x] input.ts
	+ [x] log2.ts
	+ [x] mapArray.ts
	+ [x] options.ts
	+ [x] sign/schnorr.ts
	+ [x] crypto/ncryptsec.ts
	+ [x] db.ts
	+ [x] inputTab.ts
	+ [x] mutex.ts
* [x] input.tsから送信するパスワードをUint8Arrayにする
	+ [x] crypto/encryptsec.tsのメソッドの引数をNFKC後のUint8Arrayにする
		- [x] encodePassword関数を作成
		- [x] encodePassword関数を使用
		- [x] encodePassword関数をoptions.js側にコピー
		- [x] generateを修正
		- [x] encodePassword関数をbackground側にコピー
		- [x] encodePasswordをbackground側でするようにする
		- [x] encodePasswordをinput.ts側でするようにする
		- [x] encodeされたpasswordを0クリアするようにする
			* [x] input.ts側で
			* [x] background側で
* [x] input.tsからbackground.tsへパスワードを送信する経路をセキュアにする
	+ [x] registerSymmetricKeyメッセージを受け取ったら、senderのタブにportを接続する
	+ [x] input.ts側からportを使ってパスワードを送信する
	+ [x] パスワードを受信する
* [x] optionsState.jsの仕様を変更する(途中)
	+ [x] TypeScriptにする
	+ [x] この時点でできるならexec/test-options-state.jsを.tsにする
* [x] password errorをhiddenの切り換えにする
* [x] 設定画面の実装の変更(途中)
	+ [x] Backup, All Clear, Restoreボタンを作成
	+ [x] OptionsStateにStorage interfaceを使う
	+ [x] defaultOptionsObjectを定義する
* [ ] background.tsからoptions.tsで"browser"のときに接続エラーが出るあたり確認
* [x] 設定画面の実装の変更(続き, 途中)
	+ [x] Backupボタンの処理を書く
		- [x] options.tsからbackground.tsにreadClientListDevを送る
		- [x] background.tsはIndexedDBからclient listを入手して返信として返す
		- [x] clientsをうめる
		- [x] 他をうめる
	+ [x] OptionsObjectを引数に取り、設定画面のほうを修正して同期させる関数を定義する(途中)
		- [x] accountNameからpasswordErrorまで
		- [x] clientsElmからclientDetailまで
		- [x] type Clientを独立したモジュールにうつす
* [x] clientsのbackupのデータの流れを修正する
	+ [x] それぞれのclientにidとしてUUIDを付加する
	+ [x] HTML側からOptionsObject側にデータをコピーするようにする
* [x] currentKeyのoptionのvalueをnpubからhexにする
* [x] OptionsObjectにcurrentKeyOptionsを追加する
	+ [x] curerntKeyOptionsの型を考える
	+ [x] 追加する
	+ [x] currentKeyDの子要素についてmapする
	+ [x] 周辺の修正
* [x] loadPublicKeysのcloneNodeを消す
* [x] select内のpublicKeyの読み込みについて考える
* [x] editingClientを消す
* [x] 設定画面の実装の変更(続き、途中)
	+ [x] OptionsObjectを引数に取り、設定画面のほうを修正して同期させる関数を定義する(続き)
		- [x] optionsObjectToClientDetail関数を定義
		- [x] clients
		- [x] clientNameからdetailErrorまで
		- [x] optionsErrorとuseClientSet
	+ [x] All Clearボタンの処理を書く
	+ [x] Restoreボタンの処理を書く
	+ [x] type EditingClientをtypesに移動する
	+ [x] clientクリック時の処理を修正する
		- [x] backgroundにメッセージを送りdetail用のデータを入手する
		- [x] 入手したデータでフォームを更新する
	+ [x] clients
		- [x] All Clearボタンの処理を書く
		- [x] Restoreボタンの処理を書く
	+ [x] options.tsをざっくりと読み細かいところを修正 (途中)
		- [x] GET ELEMENT
		- [x] INITIALIZATION
			* [x] backgroundにメッセージを送る
			* [x] openNewClientから処理を消していく
				+ [x] clientDetail.dataset.clientUuid
				+ [x] publicKeys
				+ [x] deleteClient.hidden
				+ [x] clientsElm.hidden
				+ [x] newClient.hidden
				+ [x] clientDetail.hidden
				+ [x] optionsError.textContent
				+ [x] defaultEditingClient
				+ [x] loadClientToForm
			* [x] openNewClient()を削除
			* [x] background側でObjectOptions型の値を作り返す
			* [x] loadOptionsに上の値をあたえる
			* [x] 今までの初期化で不要なものを消す
			* [x] その他
	+ [x] isUUID()を定義
	+ [x] isUrl()を定義
	+ [x] type OptionsTab = { openType: string, id: string }といった型を定義する
	+ [x] InputTabsクラスに逆引き関数を定義
	+ [x] InputTabsインスタンスを使って、
		sender tab IDからOptionsTabを生成する関数を書く
	+ [x] browser options tabをInputTab管理下にするコードを移動させる
* [x] OptionsStateを実装していく(途中)
	+ [x] openType == "url"の場合のhiddenとerrorの設定を関数にまとめる
* [x] アカウント名のところをbackground側とのやりとりをするようにする
	+ [x] input eventでbackgroundにメッセージを送る
	+ [x] backgroundはメッセージを受け取りOptionsStateを操作する
	+ [x] OptionsStateはOpitonObjectを更新する
* [x] OptionsStateのバックアップやリストアのところを作る
	+ [x] Mapを作る
	+ [x] idからOptionsObjectを作る処理を関数化する (今のところやらない)
	+ [x] background.tsの起動時に要素を追加する
		- [x] storage.sessionにあればOptionsObjectを復元する
	+ [x] Mapの要素をidで検索
	+ [x] OptionsBeginでMapに要素(OptionsStateのインスタンス)があれば
		- [x] OptionsObjectをoptions.tsに返す
	+ [x] OptionsBeginでMapに要素がなければ追加する
		- [x] OptionsObjectを作り
		- [x] それをOptionsStateに記録し
		- [x] storate.sessionにバックアップを取り
		- [x] options.ts側に返す
		- [x] OptionsStateをMapに記録する
* [x] generateボタンの実装を修正
	+ [x] パスワードの送信
		- [x] stringからuint8Arrayに変換する
		- [x] port接続
		- [x] 送信
		- [x] 受信
	+ [x] 鍵ペアをbackgroundで生成する
	+ [x] 鍵ペアをbackgroundで登録する
* [x] 鍵ペアを作ったときにアカウントリストをupdateする
* [x] OptionsStateを使う形に修正する
	+ [x] Client Detail
		- [x] client name
		- [x] senderからOptionsStateのインスタンスを
			検索する関数を作成する
		- [x] client URL pattern
		- [x] Use priority for Client selection
		- [x] Priority
		- [x] Display account
		- [x] Horizontal position
		- [x] Vertical position
		- [x] Color
		- [x] Opacity
		- [x] Open settings by clicking the account display
		- [x] Public Key
* [x] ボタンの修正 (途中)
	+ [x] delete
		- [x] backgroundにメッセージを送る
		- [x] 処理をbackgroundに移していく
	+ [x] apply
		- [x] backgroundに処理を移す
		- [x] options.tsのapplyClient()をきれいにする
			* [x] 前提条件の部分をまとめる
				+ [x] isClientMatchedがResultを返すようにする
				+ [x] まとめる
			* [x] 必要ならタブ管理により閉じる部分
			* [x] 名前等含めリファクタリングする
	+ [x] cancel
	+ [x] submit
* [x] optionsObjectからshowPasswordを削除する
* [x] optionsObjectからpasswordErrorHiddenを削除する
* [x] クライアントリストを名前順に並べる
* [x] アカウントリストを名前順に並べる
* [x] 開発用のボタンを削除する
* [x] Use Client Setボタンとクライアントリストとアカウントリストの同期
	+ [x] 三者をデータベースから読み出す関数を作成
	+ [x] backgroundからupdateメッセージを送る(from: "background"とか)
		- { class: "Options", direction: "signal", method: "updated" }
	+ [x] updateリクエストをoptions.tsからbackground.tsに送る
	+ [x] updateメッセージを受け取り読み出して表示する
	+ [x] 他の場所でもbackgroundからupdateメッセージを送る
		- [x] クライアントの作成(Apply/OK)
		- [x] クライアントの変更(Apply/OK)
		- [x] クライアントの削除(Delete)
		- [x] アカウントの作成(Generate)
* [x] notifyUseSetClientChangeを削除する
* [x] クライアント詳細画面を全体で1個だけにする
	+ [x] optionsStateで引数のUUIDの詳細画面が開いているかを調べる(booleanを返す)関数を作る
	+ [x] findKey()を定義する
	+ [x] クライアント詳細画面を開く前に既存のものがあるか探す
		- optionsStatesのvaluesを検索
	+ [x] 見つかった場合、そのIDを表示
	+ [x] 同上、そのタブIDを表示
	+ [x] あれば、それのタブをアクティブにする
	+ [x] なければそれまでの処理にもどる
	+ [x] 元画面では「警告」表示をpopoverする
	+ [x] アカウント表示領域から開いたときのシナリオについて作成する
* [x] アカウント表示領域から開いた設定画面がクライアント詳細画面を開くようにする
	+ [x] opentype = "uuid"のifを書く
	+ [x] 既存の詳細画面があるか調べる
	+ [x] クライアント詳細画面で同じものがあればそのタブをアクティブにする
	+ [x] その他
* [x] ボタンの修正 (続き)
	+ [x] Revertボタンの作成
	+ [x] 順番を変える。Revert/Apply/Cancel/OK
	+ [x] openType == "url"のときRevert/Apply/Cancelボタンをhiddenにする
		- [x] revertButtonHidden, applyButtonHidden, cancelButtonHiddenをOptionsObjectに
* [x] 設定画面が閉じたときにoptionsStatesから削除する処理が必要
	+ [x] そうではなくて使用側で無視するようにした
* [x] データベースを全消去したあと、全タブをreloadする
* [x] データベースの更新処理を終理する
	+ [x] v0 -> v1を関数化する
	+ [x] v2 -> v3を関数化する
	+ [x] v3 -> v4を関数化する
* [x] Set機能の追加
	+ [x] DBの修正
		- [x] clientテーブルをclientとsetClientテーブルとに分ける
			* [x] clientsテーブルの名前をsetClientsテーブルに変更する
			* [x] 既存のデータベースからclientテーブルを作成する
				+ UUID, 名前, URLPattern
		- [x] setClients -> setClientsTmp
			* [x] データの移行
			* [x] setClientsの削除
			* [x] 読み書きにsetClientsTmpを使うようにする
		- [x] setsテーブルを作る
		- [x] currentSetテーブルを作る
		- [x] currentSetテーブルへのput関数を作る
		- [x] setsテーブルへのput関数を作る
		- [x] データベースの更新処理のところでcurrentSetとsetsへ値を追加する
		- [x] set-clientsテーブルを複合主キーで作る
		- [x] 既存のデータベースのsetClientsTmpからname, urlPatternを消しているか確認
			* [x] 消していなければversionをupして消す <- 消してあった
		- [x] putClientでname, urlPatternを消す
		- [x] setClientTmpのエントリーであるオブジェクトの型を調べる
		- [x] 型SetClientを定義する
		- [x] 関数putSetClientを定義する (:: SetClient -> IO ())
		- [x] 関数mkSetClientを定義する (:: SetClientOld -> SetClient)
		- [x] setClientsTmpテーブルからset-clientsテーブルにコピーする
			* setUuidにはcurrent-setの値を使う
		- [x] setClientsTmpテーブルからset-clientsテーブルを使うようにする
			* setUuidについては必要な部分を実装する
		- [x] setClientsTmpテーブルを削除する
		- [x] default setひとつという形にする
	+ [x] DB側でsetClientとclientを分離した作りにする
		- [x] putClient
		- [x] deleteClient
		- [x] getClients
		- [x] getClientSummaries
		- [x] getClient
	+ [x] DB側でsetを意識した作りにする
		- [x] putClient
		- [x] deleteClient
		- [x] getClients
		- [x] getClientSummaries
		- [x] getClient
	+ [x] 利用する側でsetを意識した処理にする
	+ [x] setを設定する画面を作る
		- [x] 画面の移行だけを実装
			* [x] use client set時だけ見えるSelect Setボタンを追加
				+ [x] HTMLに追加
				+ [x] loadOptionsでhiddenを管理する
					- OptionsStateのUseClientSetの項目を利用する
				+ [x] Select Setボタンで設定画面に対するbroadcastを発火させる
			* [x] OptionsStateにset領域とnot-set領域のhiddenを追加
			* [x] Select Setボタンでセット選択画面に移行
			* [x] セットのリストを表示
				+ [x] DB.getSetListを定義
				+ [x] 型ClientSetをtypes.tsに移動
				+ [x] OptionsObjectに追加
				+ [x] OptionsStateで追加する処理を定義
				+ [x] initializeのときにデータベースから読み込む
				+ [x] loadOptionsで表示
			* [x] セットのリストの要素からメッセージを送る
			* [x] ラジオボタンの内容の一部を実装
			* [x] ラジオボタンがSelect & Editのときには
				選択により設定画面がもどるようにする
		- [x] New Setボタンの内容を実装
			* [x] HTMLに追加
			* [x] options.tsからメッセージを送信
			* [x] background.tsからデータベースに登録
		- [x] 現在のSet以外を薄く表示
		- [x] ラジオボタンの内容を実装
		- [x] 選択肢をクリックしたときの処理を実装
	+ [x] Setにないクライアントの設定画面を出す
		- newClientと似た処理にするが次のものを初期設定済みとする
			* クライアントUUID
			* クライアント名
			* URLPattern
		- [x] new clientの処理を関数化
		- [x] new clientの処理のうちuuidを生成する処理を関数外に出す
		- [x] new clientとurl clientとnot-set clientに共通の関数を作る
		- [x] new clientとurl clientにその関数を使う
		- [x] not-set clientを作るのにその関数を使う
	+ [x] アカウント表示領域からid = "set"で開くようにする
	+ [x] 選択をしたら元タブにもどり閉じる
	+ [x] 上記の時にちゃんとページのupdateをするようにする
	+ [x] 既存のdetail画面があっても"set"のときは気にしないようにする
	+ [x] contentTabsをbrowser.storage.sessionにバックアップ、リストア
		- [x] storage.sessionを使う
		- [x] 排他制御
	+ [ ] クライアントリストで現在のセットで使われていないものを薄字にする
	+ [ ] DBから不要な項目を削除
		- [x] setClientテーブルから名前とURLPatternを削除する
		- [ ] その他
	+ [x] setを使用する場合にはaccount表示領域のクリックでそれを開く
* [x] priorityをsetClientsテーブルからclientsテーブルに移動する
	+ [x] priorityをclientsテーブルにコピーする
	+ [x] priorityがコピーされていることを確認する
	+ [x] priorityについてclientsテーブルを参照するようにする
	+ [x] setClientsテーブルからpriorityを消す
* [ ] UUIDのOptionsページの仕様を修正する
	+ [x] UUIDと「URLのセット」の辞書を作る
	+ [ ] 関数removeOptionsSourceTab(tabId)を作る
	+ [ ] background.tsのcase "openSettings"のところをリファクタリングする
	+ [ ] 既存のクライアント詳細画面を開く処理をif(use set)に対するelseのところに入れる
	+ [ ] UUIDで設定画面を開いたらそれに追加する
	+ [ ] applyの時に、全部のURLをマッチさせられることを確認する
* [x] checkUrlPatternあたりを整理する
	+ [x] チェックをbackground.ts側にうつす
		- [x] checkUrlPatternを関数化する
		- [x] isClientMatchedを関数化する
		- [x] applyClientを関数化する
		- [x] clientSubmitedを関数化する
		- [x] OptionsStateにsetDetailErrorHiddenを定義する
		- [x] OptionsStateにsetUrlPatternNotMatchErrorHiddenを定義する
		- [x] background.ts側でチェックするようにする
		- [x] applyClientに引数を追加してApplyとOKを分けるようにする
		- [x] loadClientSummariesあたりについて調べる
		- [x] options.ts側でのチェックを消す
		- [x] エラーメッセージの表示を修正する
			* [x] background.ts側のエラーメッセージ
			* [x] loadOptionsでエラーについてもformに反映する
		- [x] エラーメッセージの消去をbackground.ts側に移す
			* [x] documment.addEventListenerでbackgroundにメッセージを送る
			* [x] background.ts側でエラーメッセージを消去
			* [x] options.ts側でloadOpitons()する
* [x] クライアントが存在しなかった場合、Profileが存在しなかった場合の設定画面のbackgroundを書く
	+ [x] HTMLにボタンを追加
	+ [x] options.tsからbackground.tsにメッセージを送る
	+ [x] メッセージを受け取ったらURLPatternの文法をチェック
		- [x] エラーメッセージを表示
	+ [x] URLPatternがマッチするかをチェック
	+ [x] マッチしなければエラーメッセージを表示する
	+ [x] DBにclientをputする関数を追加する
	+ [x] OptionsStateからClientSummaryRawを取り出すメソッドを書く
	+ [x] マッチしたらDBに保存して次の段階に進むが、ここは今のところ「そのまま」とする
	+ [x] Profile型を定義する
	+ [x] DBにProfileだけ保存する関数を追加する
	+ [x] OptionsStateにprofileを取り出すメソッドを追加する
	+ [x] 元々のOKを押すとprofile部分だけをDBに保存する
* [x] クライアントが存在し、Profileが存在しない場合の設定画面のbackgroundを書く
* [x] クライアントの設定項目とProfileの設定項目とで分けて&lt;div&gt;でくくる
* [x] optionsObjectに追加
	+ [x] clientSettingsHidden
	+ [x] profileSettingsHidden
* [x] optionsStateに追加
	+ [x] clientSettingsMode
	+ [x] profileSettingsMode
* [x] options.tsに追加
	+ [x] clientSettings.hidden
	+ [x] profileSettings.hidden
* [x] クライアントリストの下にラジオボタンを配置する
	+ [x] Client
	+ [x] Profile
* [x] ラジオボタンをクライアント詳細画面では消す
	+ [x] ラジオボタンを消すための値をoptionsObjectに追加
	+ [x] それをoptionsStateで使う
	+ [x] loadOptionsで消すようにする
* [x] background側にラジオボタンの状態を意味する値を追加する
	+ [x] optionsObject
	+ [x] optionsSettings
* [x] ラジオボタンによりクリックの意味を変える
* [ ] clientsテーブルとset-clientsテーブルの要素を確認する
* [x] 「権限テーブル」機能の追加の第一歩目としてクライアントの作成と編集を分離させる
* [x] 設定項目をClient, Account, Client Profileの3項目に分ける
* [x] 「クライアンのprofileが存在しなかった場合」の処理を書く
* [x] クライアントの権限テーブル (途中)
	+ [x] データベースを更新する
		- [x] テーブルを作る関数を定義する
		- [x] テーブルに保存したり、引き出したりするための型を用意する
		- [x] 権限を追加する関数を定義する
		- [x] 権限を読み出す関数を定義する
		- [x] 既存のクライアントとアカウントの全ペアについて最大限の権限を追加する
	+ [x] データベースへの書き込み関数を定義する
	+ [x] permissionsにclientUuidのindexを作る
	+ [x] データベースから特定のクライアント用の
		Permissionのリストを取り出す関数getPermissionsByClientUuidを作成する
	+ [x] 権限を定義するUIを作る (途中)
		- [x] 1行を意味する型を作る - Permission
		- [x] HTMLにラジオボタンPermissionを追加する
		- [x] HTMLに表示用の領域を作る
		- [x] PermissionNewクラスを作成する
		- [x] OptionsObjectに保存する型をPermissionから変更する
			* publlic keyだけではなくnameも必要
		- [x] permissionSettingsHiddenを追加する
		- [x] HTMLにExit, Nextボタンを作成
		- [x] Exit, Nextボタンの機能を作成
			* [x] background.tsへのsendMessage
				- [x] Exit
				- [x] Next
			* [x] background.ts内での処理
				- [x] Exitボタンの処理にはclientsElmの内容の更新も必要かも
				- [x] Nextボタンは単にProfile設定画面に遷移で良い
		- [x] クライアント作成でOKにした場合にパーミッション編集画面に遷移
		- [x] クライアントを作成する流れで呼ばれた時にはExitボタンを隠す
			* [x] permissionSettingsExitHiddenをoptionsObjectに設定
			* [x] setPermissionSettingsExitHiddenをoptionsStateに設定
			* [x] setPermissionSettingsExitHiddenをbackgroundに追加
			* [x] loadOptionsにpermissionSettingsExitHiddenを追加
		- [x] loadOptionsで表示する
			* [x] ラジオボタンを表示する
	+ [x] ラジオボタンの機能を書く
		- [x] backgroundにメッセージを送る
		- [x] permissionからラジオボタンのvalueを計算する関数を書く
		- [x] ラジオボタンの状態をloadする
		- [x] 保持するpermissionsを配列ではなく辞書にする
		- [x] ラジオボタンの値からpermissionを計算する関数を書く
		- [x] optionsStateに保持
		- [x] DBへの書き込み
	+ [x] Nextのときにすくなくともひとつのパーミッションがnone以外であることを
		遷移の条件にする
		- [x] チェックして進まなくする
		- [x] エラーメッセージを表示する
	+ [x] Profileの設定のときに権限のないアカウントは表示しないようにする
		- [x] DB.getPublicKeyOptionsの引数にクライアントUUIDを追加する
		- [x] DB.getPublicKeyOptionsの中でfilteringする
	+ [x] 権限を定義するUIを作る (続き、途中)
		- [x] クライアントを作成する流れでない場合に
			クライアント編集画面に以下のボタンを置く(途中)
			* Revert/Apply/Cancel/OK
			* [x] HTMLに設定
			* [x] CSSの設定
		- [x] Permission[]から画面を表示する関数を書く
		- [x] PermissionsをoptionsObjectに
		- [x] PermissionsをoptionsStateに
		- [x] Permissionsを読み込む処理を書く
* [x] Deleteの実装を決める
* [x] クライアントの設定とプロファイルのDeleteについて考える
* [x] Delete機能の新実装 (途中)
	+ [x] 確認画面についてhiddenのfalse, trueを切り換える仕組みを作る
	+ [x] Deleteボタンで画面の遷移を行う
	+ [x] Cancelボタンで画面をもどる
	+ [x] Deleteボタンで下で作った確認画面を開かないようにする
	+ [x] set-clientsテーブルにclientUuidでindexをつける
	+ [x] DB.deleteClientで関連するテーブルの要素も削除する
		- [x] set-clientsテーブルの要素を削除する
		- [x] permissionsテーブルの要素を削除する
	+ [x] OKボタンでDeleteを実行しClientList modeに移行する
		- [x] 前に用意したメッセージを単に送るだけでいいか考える
		- [x] background.tsのdeleteClientConfirmedを修正する
			* [x] tab IDは必要ない
			* [x] client UUIDもメッセージで渡す必要はない
		- [x] 実装する
* [x] 存在しないクライアントを作成する流れでない場合に
	クライアント編集画面に以下のボタンを置く(続き)
	+ Revert/Apply/Cancel/OK
	+ [x] backgroundにmessageを送る
		- [x] Revertのメッセージを送る("clientSettingsRevert")
			* [x] 何もせずoptionsObjectを返す処理をbackgroundに書いておく
			* [x] メッセージを送る
		- [x] Applyのメッセージを送る
			* [x] ファームのバリデーションをチェックする
			* [x] メッセージを送る
		- [x] Cancelのメッセージを送る
	+ [x] background側で処理を書く
		- [x] Revertの処理を書く
		- [x] Applyの処理を書く
			* [x] URLPatternが文法的に正しいことをチェックする
				+ [x] 正しくない場合にはエラーメッセージを表示する
			* [x] DBに書き込む
		- [x] Cancelの処理を書く
* [x] openClientDetailのDB.getClient()のところを修正する
* [x] revertのDB.getClient()のところを修正する
* [x] クライアントの設定あたりのvalidationについて整理する
	+ [x] formを分ける
* [x] クライアント作成する流れの場合にはOKボタン以外は隠す
	+ 消すのはDelete/Revert/Apply/Cancel
	+ [x] optionsObjectにそれぞれのhiddenを追加
	+ [x] options.tsでそれを反映するようにする
	+ [x] optionsStateでそれらをまとめてhiddenにするメソッドを作成
		名前はonlyClientOkあたりか
	+ [x] "url"のところで上記のメソッドを呼ぶ
* [x] Set選択画面でselect&editのときには消えないようにする
* [x] クライアントの権限テーブル (続き)
	+ [x] 実際に公開鍵の取得のときに権限を確認する
		- [x] データベースからbooleanを取得
		- [x] types.tsにErr型を定義する
		- [x] orErrを定義
		- [x] 権限が足りないときにはエラーメッセージをrejectの引数として返す
		- [x] 画面にpopover機能で表示する
	+ [x] 実際に署名のときに権限を確認する
		- [x] データベースからbooleanを取得
		- [x] falseならreject
		- [x] falseならpopoverで表示
	+ [x] エラーメッセージでreloadするようにアドバイスする
* [x] profile settingsのところのDeleteボタンを消す
* [x] Delete機能の実装 (中断)
	+ [x] confirmation.htmlを作成する
	+ [x] background.tsでctbsを作成する
	+ [x] confirmation.tsを作成する
	+ [x] background.ts側に無条件でmessageを送る
	+ [x] ユーザーへのメッセージをbackground.ts側から指定したものにする
* [x] クリック透過を設定可能にする
* [x] portを使ったlogの処理をテストしてみる(portは使わないことにする)
* [x] OptionsStateインスタンスをbackgroundが復元するところを書く
* [x] optionsStateのaccountの部分を実際に使ってみる
	+ [x] 初期化メッセージを送ってきた設定画面についてOptionsStateインスタンスを生成する
	+ [x] 実際に写しを取ってみる
	+ [x] generateボタンを押すとbackground側にメソッドを送る
	+ [x] 実際に動くようにする
	+ [x] 古いほうの仕組みを消す
	+ [x] 試す
* [x] options.jsから直接データベース処理をするのではなくbackgroundに依頼するようにする
	+ [x] 設定画面の「状態」を表すオブジェクトを設計する
		- [x] memo.ja.md上で
		- [x] コード上で
	+ [x] storage.session上の状態をinputなどで変化させる
	+ [x] storage.session上の状態から復元する
	+ [x] OKのときstorage.session上の状態からデータベースに書き込む
	+ [x] Cancelではstorage.session上の状態を削除する (必要ないかもしれない)
	+ [x] Deleteではデータベース上からエントリーを削除する
	+ [x] パスワード入力時の処理を記述
	+ [x] options.jsからデータベースをいじる処理を削除する
* [x] アカウント表示領域のクリックを設定可能にする(続き)
	+ [x] 設定によってクリックを透過させるかどうかを決める
	+ [x] 文字色を選べるようにする
	+ [x] 文字の不透明度を設定できるようにする
* [x] 未登録クライアントから誘導される設定画面タブの管理(続き)
	+ [x] Promiseで待ちに入るようにする
	+ [x] URLで呼ばれたときにはNew Client画面からCancelボタンをhiddenにする
	+ [x] OKが押されたら失敗か成功かテストする
		- [x] 失敗ならメッセージを送る(メッセージの返り値か)
		- [x] 失敗の場合ユーザーに対してエラーメッセージを表示する
		- [x] エラーメッセージはユーザーが入力を始めたら消えるようにする
		- [x] 成功ならInputTabs.completeとしタブを閉じる
	+ [x] InputTabs.tabClosed
		- [x] タブか閉じたことを検出しそれがURLなら元ページにエラーを送る
		- [x] エラーを受け取った「元ページ」はpromiseをrejectする
* [x] editingClientではなくeditingClientUuidとする
	+ 編集のときクライアント全体ではなく、そのUUIDだけモジュールスコープの変数に保持すればいい
	+ この話は消えたかと思う
* [x] 設定画面に「タブで開く」ボタンをつける
* [x] Android版FirefoxでsignEventで
	パスワード入力用画面を消したときにRabbitが操作できなくなるバグについて
	原因を精査する
	+ queueMicrotaskではNG
	+ setTimeout(..., 0)はOK
* [x] アカウント表示領域の文字の透過度を背景の透過度に合わせる
* [ ] ncryptsecのエクスポート
	+ [x] 今のAccount領域にNew Accountボタンを追加する
	+ [x] 今初めから出ている画面をそのボタンで開くようにする
		- [x] optionsObjectにgenerateFormHiddenを追加する
		- [x] optionsObjectにaccountListHiddenを追加する
		- [x] HTMLで&lt;div id="account-list"&gt;を追加する
		- [x] optionsStateでそのモードに遷移するメソッドを書く
		- [x] options.tsでhiddenを画面に反映する
		- [x] New Accountボタンのクリックで遷移するようにする
		- [x] Exitボタンを作る
		- [x] Exitボタンでもどれるようにする
		- [x] GenerateボタンとExitボタンとを右寄せにする
	+ [x] アカウントリストを表示する
		- [x] DB.getAllAccountsを定義する
			* DB.getPublicKeysWithNamesで定義済み
		- [x] 上記の結果をoptions画面の初期化処理のところでconsole.logする
		- [x] HTMLに表示領域を作成(#account-list-area);
			* [x] textcontentsに適当な文字列を入れてみる
		- [x] アカウントの配列をoptionsObjectに追加
		- [x] アカウントの配列を読み込むメソッドをoptionsStateに追加
		- [x] options画面の初期化処理にアカウントの配列の追加を追加
		- [x] rowをdivではなくbuttonにする
		- [x] CSSでボタンらしくない見た目にする
		- [x] options.tsでアカウントリスト表示処理を追加
			* [x] 表示
			* [x] それぞれの要素にクリックしたときにメッセージを送る処理を追加
				+ "accountClicked"でname, publicKeyを送信する
		- [x] アカウントのgenerateにアカウントの配列のoptionsStateへの追加を追加
			* [x] DBからそれぞれのフォームに反映する関数をbackground.tsに定義する
				+ [x] Account List
				+ [x] Permission List
			* [x] 別の設定画面でも反映されるようにする
				+ [x] Account List
				+ [x] Permission List
		- [x] generateしたらnew account画面を抜けるようにする
	+ [x] ラジオボタンを配置する(Rename/Export/Delete)
		- [x] HTMLに追加
		- [x] optionsObjectに追加
		- [x] optionsStateに追加
		- [x] backgroundにメッセージを送信
		- [x] background側での処理
		- [x] loadOptionsで反映させる
	+ [x] client setのedit modeを設定画面を閉じてから開いたときに復元させる
	+ [x] Rename機能を追加
		- [x] db.tsに追加
		- [x] HTMLの画面を追加
		- [x] 画面の遷移のところを作成(optionsObject, optionsState, options)
			* [x] optionsObject
				- [x] accountRenameHidden
			* [x] optionsState
			* [x] options
		- [x] accountListのクリックで遷移させる
		- [x] Cancelでaccount list modeにもどる
		- [x] 名前とPublicKeyの入力
			* [x] 名前の初期値
				+ [x] optionsObject
				+ [x] optionsState
				+ [x] options
				+ [x] background
			* [x] PublicKeyの表示
				+ [x] optionsObject
				+ [x] optionsState
				+ [x] options
				+ [x] background
				+ [x] backgroundが落ちても消えないようにする
		- [x] 名前のinputの部分
			* inputのたびにoptionsStateにバックアップする
		- [x] Cancel/OKボタン
			* [x] Cancelボタンの処理
			* [x] OKボタンの処理
				+ [x] options.ts: backgroundへのメッセージ
				+ [x] optionsState
					- [x] get name
					- [x] get public key
				+ [x] backgroundでの処理
		- [x] add Account Public Key
			* [x] optionsObject
			* [x] optionsState
			* [x] background
			* [x] options
		- [x] remove Account Rename Public Key
		- [x] 別のウィンドウの設定画面やcontent画面でもリアルタイムに変更が反映されるようにする
			* [x] content tab
			* [x] options tab
		- [x] OKボタンで画面をぬけるようにする
	+ [ ] ExportがONになっているときにExport確認画面に遷移させる
		- [ ] HTMLにExport確認領域を追加
		- [ ] optionsObjectにhiddenを追加
		- [ ] optionsStateにmodeを追加
		- [ ] loadOPtionsで反映させる
	+ [ ] Exoprt確認画面でOKを押すとダウンロードする
		- [ ] download権限を追加
		- [ ] ncryptsecの中身の文字列を作成
		- [ ] ダウンロード(ファイル名は[名前][下線][10文字][下線][6文字].ncryptsecか)
	+ [ ] completeになったところでDBにExportの時間を保存する
		- [ ] completeを確認
		- [ ] DBにダウンロードの日時を追加
* [x] TODOを修正する
* [ ] アカウント名に保存名を追加する
	+ [x] 表示名からアカウント名を計算する関数を書く
		- [x] Haskellで書く
		- [x] tools.tsで書く
	+ [x] DBの関数を定義
		- [x] getSaveName
			* public keyを指定してsave nameを取り出す
			* save nameが定義されていなければdisplay nameから生成する
		- [x] putSaveName
			* public keyを指定してsecret key全体を取り出す
			* 取り出したsecret keyにsave nameを追加する
			* 追加した結果をDBに書きもどす
	+ [ ] generateにチェックボックスと入力欄を付ける
		- [x] HTMLにチェックボックスと入力欄を追加する
			* set-custom-save-name
			* custom-save-name-label
			* custom-save-name
		- [x] チェックボックスの内容をoptionsObjectに追加
		- [x] 入力欄のhiddenをoptionsObjectに追加
			* customSaveNameLabelHidden
		- [x] チェックボックスの処理をoptionsStateに追加
			* [x] ここでチェックの内容とhiddenとを設定する
		- [x] options.tsのloadOptionsでチェックボックスとhiddenとを反映させる
		- [x] チェックボックスの変化をoptionsからbackgroundにメッセージで通知
		- [x] backgroundでの処理を書く
		- [x] 入力処理を書く
			* [x] optionsObjectに追加
			* [x] optionsStateに追加
			* [x] optionsでloadする
			* [x] setSaveNameがfalseのあいだは
				nameの入力に追随するようにする
			* [x] optionsにlistenerを追加
		- [x] Accountにsave nameが変まれていなければ、それを追加する処理を書く
		- [x] DBからAccountを取得する関数にsave nameも追加する
		- [x] Accountにsave nameを追加する
		- [x] DBにsave nameも含めた保存関数を書く(修正する)
		- [x] 保存処理を書く
			* generateボタンに対する処理に追加する
		- [x] renameのところで保存されていることを確認する
		- [ ] チェックボックスについてsaveが必要か調べる
		- [ ] 入力欄についてsaveが必要か調べる
	+ [ ] renameにチェックボックスと入力欄を付ける
		- [x] HTMLに追加
		- [x] チェックボックスのところの処理
			* [x] チェックボックスの状態の保存と復元
				+ [x] optionsObject - setSaveNameRename
				+ [x] optionsObject - customSaveNameLabelRenameHidden
				+ [x] optionsState
				+ [x] opitions
				+ [x] background
			* [x] 入力欄のhiddenとunhidden
		- [ ] 入力欄の処理
			* [ ] optionsObject
			* [ ] optionsState
			* [ ] チェックボックスがチェックされていなければnameに追随する
				+ backgroundに定義
			* [ ] optionsでinputにlistenerをつける
			* [ ] backgroundで処理
		- [ ] 名前の変更処理
			* [ ] DBの関数の修正 renameAccount
			* [ ] 必要なら型の修正
		- [ ] チェックボックスについてsaveが必要か調べる
		- [ ] 入力欄についてsaveが必要か調べる
* [ ] npubの表示を&lt;span title=...&gt;で全体を表示する
* [ ] optionsStateのsaveについて、ちゃんと動作しているか調べる
* [ ] ncryptsecのインポート
* [ ] nsecのインポート
* [ ] TODOを見返し修正する
* [ ] Delete機能の新実装 (続き)
	+ [ ] 消すクライアントの名前とURLPatternを表示する
	+ [ ] Set機能を使っているかどうかでユーザーへのメッセージを変化させる
* [ ] Delete機能の中断した実装の掃除
	+ [x] ctbsを消す
	+ [ ] それ以外、ファイルなどを消す
	+ [ ] その他
* [ ] Profileの編集領域にクライアント名とURLPatternを表示する
* [ ] log用にタブを登録する処理を削除する
* [ ] options.tsからbackground.tsにパスワードを渡すときにUUIDでチェックする
	+ これをするかどうか検討する
* [ ] loadCommonStateのdb側とostt側とを、それぞれまとめるか考える
	+ [ ] DB.getCommonState
	+ [ ] ostt.setCommonState
	+ [ ] loadCommonStateはostt.setCommonState(await DB.getCommonState)になる
* [ ] backgroundとoptionsの間の通信の設計を考える
* [ ] options.tsのformのsubmitの部分をきれいにする
* [ ] 設定画面の実装の変更(続き)
	+ [x] openTypeやidをoptions.tsから消去する
	+ [x] 設定画面の同期
	+ [x] OptionsState.tsを作っていく
	+ [ ] background.ts再起動時のOptionsStateの復元
	+ [ ] 新しく設定画面を開いたときのIndexedDBからのOptionsStateの生成
	+ [ ] 設定画面を開き直したときの設定画面の復元
	+ [ ] 設定画面からOptionsStateへの同期
	+ [ ] 同期のときにstorage.sessionにバックアップを取る
	+ [ ] 設定画面を閉じて開くテスト
	+ [ ] clientをクリックしたときにbackgrount.tsにメッセージを送る
	+ [ ] background.tsがIndexedDBからクライアント情報を取得
	+ [ ] クライアント情報でOptionsStateを更新
	+ [ ] メッセージの返答としてOptionsObjectを送る
	+ [ ] options.tsは設定画面を更新
	+ [x] options.tsの処理をbackground.tsにうつしていく
* [ ] Client型のuuidをUUIDにする
* [ ] logの処理について実装を再度検討する
* [ ] optionPageVanishedを修正 -> optionsPageVanished
* [ ] generateしたときにsymmetric keyをキャッシュするようにする
* [ ] backgroundの受け取るメッセージをメソッドとして解釈するようにする(続き)
	+ [ ] classがundefinedではない場合には、classの名前によってswitchして、それぞれの関数を呼ぶ
* [ ] ログ表示機能を設定画面に追加する(続き)
	+ [ ] ログ表示領域をチェックボタンでon/offできるようにする
	+ [ ] 7日でログは消すようにする
	+ [ ] ユーザーがログを消せるようにする
* [ ] パスワード入力用タブの異常系のテストのために/others/try-sign.htmlを修正する
	+ [ ] ボタンを押すとイベントに署名するようにする
	+ [ ] コンテンツ部分を入力できるようにする
* [ ] パスワード入力用タブの異常系
	+ [ ] 同じタブから3回パスワードを送る
	+ [ ] 異なるタブから3つのアカウントを開く
* [ ] 設定画面のタブ管理について確認する(3)
	+ [ ] アカウント表示画面から開いた設定画面(set不使用時)
		- [ ] OKで閉じたら元の画面にもどって、そのタブを消す
	+ [ ] アカウント表示画面から開いた設定画面(set使用時)
		- [ ] 選択したら元の画面にもどって、そのタブを消す
	+ [ ] ブラウザの設定画面
	+ [ ] タブとして開いた設定画面
	+ [ ] getPublicKeyのリカバリのための設定画面
* [ ] getPublicKeysWithNamesの再検討、必ずEncryptedSecretKeyインスタンスを介するようにする
* [ ] フォームの内容をデータベースに書き込む時の検証について考える
	+ とくにBech32.decode()のところ
	+ 今のところHTML側でvalidateしてJS側では例外を発生させるので良いと考えている
* [ ] InputTabsにあるverificationについて存在価値を検討する
* [ ] 「ブラウザが開いた設定画面であることの確認」をするかしないか検討する
* [ ] let fooを使っているところを三項演算子で書き直す
* [ ] openType === "url"のときタブを閉じられたら赤字でメッセージを出す
* [ ] signEventでも設定画面を開くように(するかどうか検討)する
* [ ] クライアントが開かれているときに削除された場合の動作を確認
* [ ] ncryptsecの暗号化や復号が正しいかどうかを第三者のコードで確認
* [ ] signのほうでもクライアントやプロファイルが存在しなかったときにOptionsを呼び出す
* [ ] デフォルト色を設定可能にする
