memo
====

モジュール構造
--------------

```
Main
  +- Trials.Followbox
  |    +- Trials.Followbox.Clickable
  |    +- Trials.Followbox.Event
  |    |    +- Trials.Followbox.TypeSynonym
  |    +- Trials.Followbox.View
  |         +- Trials.Followbox.TypeSynonym
  +- Trials.Followbox.Run
       +- Trials.Followbox.Handle
       |    +- Trials.Followbox.Event
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
* [x] Trials.Followbox.RandomをMonadicFrp.Randomに移動することを検討する
* [x] MonadicFrp.Sigのバグフィックスの後始末

リファクタリング
----------------

### API

* [x] Trials.Followbox.Event
* [x] Trials.Followbox.View
* [x] Trials.Followbox.Clickable
* [x] Trials.Followbox
* [x] Trials.Followbox.Handle
* [x] Trials.Followbox.Run
* [x] Trials.Followbox.TypeSynonym

### Body

* [x] Trials.Followbox.Event
	+ [x] imports
	+ [x] structure
	+ [x] body
* [x] Trials.Followbox.Clickable
	+ [x] structure
	+ [x] body
* [x] Trials.Followbox
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] before GET USER
		- [x] GET USER and after GET USER
* [x] Trials.Followbox.View
* [x] Trials.Followbox.Handle
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] fsVersionRandomGen
		- [x] STATE
		- [x] HANDLE - FOLLOWBOX
		- [x] HANDLE - MOUSE
		- [x] HANDLE - STORE AND LOAD JSONS
		- [x] HANDLE - REQUEST DATA
			* [x] handleHttpGet
			* [x] handleCalcTextExtents
			* [x] handleGetTimeZone
		- [x] HANDLE - BROWSE
		- [x] HANDLE - BEGIN AND END SLEEP
			* [x] handleBeginSleep
			* [x] handleEndSleep
		- [x] HANDLE - RAISE ERROR
		- [x] HANDLE - HELPER FUNCTION
* [x] Trials.Followbox.Run
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] RUN FOLLOWBOX
		- [x] GET FOLLOWBOX INFO
		- [x] GET OPT
* [x] Trials.Followbox.TypeSynonym
