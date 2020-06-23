memo
====

TODO
----

* [x] ハンドルを切りかえられるようにする
	+ [x] とりあえずinterpret'を定義しよう
	+ [x] まずは、テストケースを作成する
		- [x] どんなのがいいかな
		- [x] とりあえずは単純なスリープにしようかな
* [x] retryStを定義する
* [x] マウス関連を独立したモジュールにする
	+ BoxesとFollowboxの両方から使う
	+ EventとHandleを用意
	+ Handleについては、直接handleを用意するのではなく
		Eventの変換用の関数を用意しておきloopのなかで使えるようにしておく感じか
* [x] Lockの実装
* [x] Trials.Followbox.ThreadIdをMonadicFrp.ThreadIdか、あるいはそれ以下に移動させる
* [x] Trials.LockをMonadicFrp.Lockに移動させる
* [x] MonadicFrp以下を整理する
	+ [x] MonadicFrp.EventsをMonadicFrp.Eventにする
	+ [x] MonadicFrp.EventHandleを作成
	+ [x] MonadicFrp.Lockを上記ディレクトリ下に
	+ [x] MonadicFrp.ThreadIdを上記ディレクトリ下に
		- [x] MonadicFrp.ThreadId.Typeは(とりあえず)そのままにする
	+ [x] MonadicFrp.Randomを上記ディレクトリ下に
* [x] untilの型の修正
	+ Maybe r を r にする
* [x] untilを(すこし)リファクタリング
* [x] Followboxのリファクタリング
* [x] hlintによるリファクタリング
* [x] Trials.NewRandomで「状態」をSigのなかであつかう仕組みを試す
* [x] MonadicFrp以下のリファクタリング
* [ ] Data.Type.Setのリファクタリング
* [ ] Data.UnionSetのリファクタリング
* [ ] Trials.Lockのリファクタリングなど
* [ ] Reactにだけではなく、Sigにもadjust的なものを定義することを検討
* [ ] Handleで「ひとつのループのなかに、別のモジュール由来のものを入れられる」ような仕組みを作成する
	+ キーボード入力のモジュールを作成する
	+ マウス関連のイベントとまぜることを試す
* [ ] MonadicFrp.atで結果のEither値の左右の入れ替えを検討する
* [ ] STM的な機能の実装を試す

雑論
----

### 型レベルセット

#### 草稿

* 型リストが空ならば値も空であり値が空ならば型リストも空
* 型リストが空でなければ、すくなくとも、どれかひとつの型の値は含まれる
* 型リストに含まれるそれぞれの型の値は、含まれないか、あるいはひとつだけ含まれる
* 型リストに含まれない型の値だけを追加できる関数がある
* 型リストに含まれる型の値も追加できる関数がある
	+ その場合、その型はOrdクラスのインスタンスである必要がある
	+ もともと含まれる型がない場合は、そのまま追加
	+ もともと含まれる値がない場合は、そのまま追加
	+ 含まれる値があった場合は、新しい値と含まれている値とを比較して「小さい」ほうを選ぶ

### 問題点

* 型レベルセットについて
	+ 実質的には重複を許さない型レベルソート済みリスト
	+ GHCレベルでのサポートが必要では
	+ いまはTHでtype Number Foo = 123みたいにやってる
	+ 上記のようなやりかただとFoo aみたいな型に対応できない

### 参考

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```
