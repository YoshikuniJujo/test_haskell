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
		- [ ] なければOptionsObjectを作る <- この処理が必要かどうか検討する
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
* [ ] Set機能の追加
	+ [ ] DBの修正
		- [ ] clientテーブルをclientとsetClientテーブルとに分ける
			* [x] clientsテーブルの名前をsetClientsテーブルに変更する
			* [ ] 既存のデータベースからclientテーブルを作成する
				+ UUID, 名前, URLPattern
		- [ ] setテーブルを作る
		- [ ] currentSetテーブルを作る
		- [ ] default setひとつという形にする
	+ [ ] DB側でsetを意識した作りにする
	+ [ ] 利用する側でsetを意識した処理にする
	+ [ ] 設定側でsetを意識した処理にする
	+ [ ] DBから不要な項目を削除
	+ [ ] setを変更する設定画面を作る
	+ [ ] setを使用する場合にはaccount表示領域のクリックでそれを開く
* [ ] TODOを見返し修正する
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
	+ [ ] optionsTabId()を定義
	+ [ ] options.tsをざっくりと読み細かいところを修正 (続き)
		- [x] ADD EVENT LISTENER - Client List
		- [ ] ADD EVENT LISTENER - Client Detail
		- [ ] ADD EVENT LISTENER - Account
		- [ ] ADD EVENT LISTENER - Others
		- [ ] FOR DEVELOPMENT
		- [ ] TYPES
		- [ ] FUNCTIONS
	+ [x] OptionsState.tsを作っていく
	+ [ ] type Clientとtype EditingClientを統一できるか検討する
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
	+ [ ] options.tsの処理をbackground.tsにうつしていく
* [x] クリック透過を設定可能にする
* [ ] Client型のuuidをUUIDにする
* [ ] logの処理について実装を再度検討する
* [x] portを使ったlogの処理をテストしてみる(portは使わないことにする)
* [x] OptionsStateインスタンスをbackgroundが復元するところを書く
* [ ] optionPageVanishedを修正 -> optionsPageVanished
* [x] optionsStateのaccountの部分を実際に使ってみる
	+ [x] 初期化メッセージを送ってきた設定画面についてOptionsStateインスタンスを生成する
	+ [x] 実際に写しを取ってみる
	+ [x] generateボタンを押すとbackground側にメソッドを送る
	+ [x] 実際に動くようにする
	+ [x] 古いほうの仕組みを消す
	+ [x] 試す
* [ ] generateしたときにsymmetric keyをキャッシュするようにする
* [ ] backgroundの受け取るメッセージをメソッドとして解釈するようにする(続き)
	+ [ ] classがundefinedではない場合には、classの名前によってswitchして、それぞれの関数を呼ぶ
* [ ] optionsStateのclientの部分についても同様に作る
	+ [ ] optionsState.js側を作成
	+ [ ] options.js側を作成
	+ [ ] background.js側を作成
* [ ] ログ表示機能を設定画面に追加する(続き)
	+ [ ] ログ表示領域をチェックボタンでon/offできるようにする
	+ [ ] 7日でログは消すようにする
	+ [ ] ユーザーがログを消せるようにする
* [ ] options.jsの仕様を考える
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
* [ ] 未登録クライアントから誘導される設定画面タブの管理(続き)
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
	+ [ ] 仕様を検討する
	+ [ ] テスト
	+ [ ] 残り
* [x] アカウント表示領域のクリックを設定可能にする(続き)
	+ [x] 設定によってクリックを透過させるかどうかを決める
	+ [x] 文字色を選べるようにする
	+ [x] 文字の不透明度を設定できるようにする
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
* [ ] クライアントの権限テーブル
* [ ] ncryptsecのエクスポート
* [ ] ncryptsecのインポート
* [ ] nsecのインポート
* [x] editingClientではなくeditingClientUuidとする
	+ 編集のときクライアント全体ではなく、そのUUIDだけモジュールスコープの変数に保持すればいい
	+ この話は消えたかと思う
* [ ] getPublicKeysWithNamesの再検討、必ずEncryptedSecretKeyインスタンスを介するようにする
* [ ] フォームの内容をデータベースに書き込む時の検証について考える
	+ とくにBech32.decode()のところ
	+ 今のところHTML側でvalidateしてJS側では例外を発生させるので良いと考えている
* [ ] InputTabsにあるverificationについて存在価値を検討する
* [ ] 「ブラウザが開いた設定画面であることの確認」をするかしないか検討する
* [ ] デフォルト色を設定可能にする
* [x] 設定画面に「タブで開く」ボタンをつける
* [ ] let fooを使っているところを三項演算子で書き直す
* [x] Android版FirefoxでsignEventで
	パスワード入力用画面を消したときにRabbitが操作できなくなるバグについて
	原因を精査する
	+ queueMicrotaskではNG
	+ setTimeout(..., 0)はOK
* [x] アカウント表示領域の文字の透過度を背景の透過度に合わせる
* [ ] openType === "url"のときタブを閉じられたら赤字でメッセージを出す
* [ ] signEventでも設定画面を開くように(するかどうか検討)する
