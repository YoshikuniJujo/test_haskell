# content.js

## Map

* 値の追加や削除、読み出しを関数化することを検討する

### requestsByAnser

* answerから複数のrequestへの辞書
* answerは本番環境ではpublic key
* answerを鍵にrequestの配列をvalueにするもの
    + answerを鍵にrequestを追加する関数を定義する
* パスワードが使用可能になったことを通知するために使われる
* パスワードの入力用のタブが閉じられて、
    パスワード入力に失敗したことの通知に使われる
* 名前の変更も検討
* requestは本番環境では、それぞれの「署名要求」につけられるID
* それぞれの「何かの取得」や「署名要求」に対して、
    公開鍵またはanswer idに対してbackgroundから来るreadyまたはerrorを
    それぞれのrequestに送りとどけるために使われる
* 意味合い的には「この公開鍵を使っているもの」的な名前かさそう

### pendingPassword

### newPendingRequests
