memo
====

ポートの終わり方
----------------

### background.js起点

* background.jsから終了メッセージを送る
* options.jsはフラグを設定してACKを送る
* background.jsはdisconnectする
* options.jsはdisconnectを検出しフラグを確認しdisposeする
