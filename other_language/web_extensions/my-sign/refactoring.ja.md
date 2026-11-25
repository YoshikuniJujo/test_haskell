REFACTORING
===========

* [ ] log用にタブを登録する処理を削除する
* [ ] loadCommonStateのdb側とostt側とを、それぞれまとめるか考える
	+ [ ] DB.getCommonState
	+ [ ] ostt.setCommonState
	+ [ ] loadCommonStateはostt.setCommonState(await DB.getCommonState)になる
* [ ] options.tsのformのsubmitの部分をきれいにする
* [ ] Client型のuuidをUUIDにする
* [ ] backgroundの受け取るメッセージをメソッドとして解釈するようにする(続き)
	+ [ ] classがundefinedではない場合には、classの名前によってswitchして、それぞれの関数を呼ぶ
* [ ] getPublicKeysWithNamesの再検討、必ずEncryptedSecretKeyインスタンスを介するようにする
* [ ] let fooを使っているところを三項演算子で書き直す
* [ ] DBから不要な項目を削除
* [ ] clientsテーブルとset-clientsテーブルの要素を確認する
