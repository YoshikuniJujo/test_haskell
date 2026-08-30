# content.js

## Map

* 値の追加や削除、読み出しを関数化することを検討する

### requestsWaitingForAnswer

* answerから複数のresolve, rejectへの辞書
* answerは本番環境ではpublic key
* answerを鍵にresolve, rejectの配列をvalueにするもの
    + answerを鍵にresolve, rejectを追加する関数を定義する
* パスワードが使用可能になったことを通知するために使われる
* パスワードの入力用のタブが閉じられて、
    パスワード入力に失敗したことの通知に使われる
* 名前の変更も検討
* それぞれの「何かの取得」や「署名要求」に対して、
    公開鍵またはanswer idに対してbackgroundから来るreadyまたはerrorを
    それぞれのrequestに送りとどけるために使われる
* 意味合い的には「この公開鍵を使っているもの」的な名前かさそう
* 名前はrequetsUsingAnswerあたりに変更しようかと
* 名前はrequestsWaitingForAnswerとした

### pendingPassword

* 名前はlegacy
* 本番では署名であるような何かを要求し、backgroundがそれをメッセージで返す
* 返されたメッセージかな値を取り出し、大元のPromiseの返り値にする
* そのためにユニークなidとresolveとの対応を保持する
* 名前を変える必要がある
* 「何か」への要求と、それを受け取る「場所」を関連づけるもの
* 本番なら署名を受け取る場所みたいな感じ
* ここでは「何か」を受け取る場所のような感じ
    + たとえばsomethingResolverのような感じか
    + 本番だとsignResolverのような感じかな
