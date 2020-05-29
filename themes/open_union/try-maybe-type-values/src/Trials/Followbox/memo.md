memo
====

モジュール構造
--------------

```
Main
  +- Trials.Followbox
  |    +- Trials.Followbox.Clickable
  |    +- Trials.Followbox.Event
  |    |    +- Trials.Followbox.Random
  |    |    +- Trials.Followbox.TypeSynonym
  |    +- Trials.Followbox.View
  |         +- Trials.Followbox.TypeSynonym
  +- Trials.Followbox.Run
       +- Trials.Followbox.Handle
       |    +- Trials.Followbox.Event
       |    +- Trials.Followbox.Random
       |    +- Trials.Followbox.TypeSynonym
       +- Trials.Followbox.Event
       +- Trials.Followbox.View
       +- Trials.Followbox.TypeSynonym
```

TODO
----

* [x] LeftClickなどではなくMonadicFrp.Events.Mouseを使うようにする
	+ [x] MonadicFrp.Events.MouseにleftClick, rightClick, middleClickを追加する
* [x] RandomでLockを使うようにする
	+ [x] Trials.Followbox.NewRandomを定義する
	+ [x] 置き換える
* [ ] Trials.Followbox.RandomをMonadicFrp.Randomに移動することを検討する

リファクタリング
----------------

### API

* [x] Trials.Followbox.Random
* [x] Trials.Followbox.Event
* [x] Trials.Followbox.View
* [x] Trials.Followbox.Clickable
* [x] Trials.Followbox
* [x] Trials.Followbox.Handle
* [x] Trials.Followbox.Run
* [x] Trials.Followbox.TypeSynonym

### Body

* [x] Trials.Followbox.Random
	+ [x] imports
	+ [x] structure
	+ [x] body
* [ ] Trials.Followbox.Event
	+ [ ] imports
	* [ ] structure
	+ [ ] body
* [ ] Trials.Followbox.Clickable
* [ ] Trials.Followbox
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Trials.Followbox.View
* [ ] Trials.Followbox.Handle
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Trials.Followbox.Run
	+ [ ] imports
	+ [ ] body
* [ ] Trials.Followbox.TypeSynonym
