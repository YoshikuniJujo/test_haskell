memo
====

todo
----

* [x] parallel freer
* [x] adjust
* [x] check leftDown \`first\` rightUp
* [x] `update :: React s es a -> ... -> React s es' b -> ...` -> `update :: React s es a -> ... -> React s es b -> ...`
	+ adjustしてからupdateする感じか
* [x] DeleteEventでちゃんと終了するようにする
* [x] Moffy.EventHandle.Random
* [x] Moffy.EventHandle.Lock
* [x] -Wallによるrefactoring
	+ [x] 警告の出るモジュールをリストアップ
	+ [x] Moffy.Handle
	+ [x] Trial.CheckRandom
	+ [x] Trial.CheckSharing.ThreadId
	+ [x] Trial.CountWithLock
	+ [x] Trial.TryCheckDup
* [x] hlintによるrefactoring
* [x] モジュールの追加
	+ [x] Control.Moffy
	+ [x] Control.Moffy.Handle
	+ [x] Control.Moffy.Run
* [x] 上記3つのモジュールを利用するようにする
	+ [x] Trial.CheckRandom
	+ [x] Trial.Count
	+ [x] Trial.CountWithLock
	+ [x] Trial.TryThreadId
	+ [x] Trial.TryCheckDup
	+ [x] Trial.CheckSaring
	+ [x] Trial.Boxes
	+ [x] Trial.StepByStepBox
	+ [x] Trial.Followbox
* [ ] refactoring

refactoring
-----------

### Moffy

#### module hierarchy

```
Control.Moffy
  +- Control.Moffy.Internal.Sig
  |   +- Control.Moffy.Internal.Sig.Type
  |   +- Control.Moffy.Internal.React
  |   +- Control.Moffy.INternal.React.Type
  +- Control.Moffy.Internal.Sig.Type
  |   +- Control.Moffy.Internal.React.Type
  +- Control.Moffy.Internal.React
  |   +- Control.Moffy.Internal.React.Type
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Handle
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Run
  +- Control.Moffy.Internal.Sig.Type
  +- Control.Moffy.Internal.React.Type
```

#### todo

* [ ] モジュールの整理
	+ [x] 現在のモジュール構造のチェック
	+ [x] モジュールの整理
		- [x] Control.Moffy.Internal.Fooに移動
		- [x] Control.Moffy.Internal.ReactとControl.Moffy.Internal.React.Commonについて
			* [x] Type, Handle, Runをきれいに分ける
				+ [x] Control.Moffy.Internal..Handle -> Control.Moffy.Handle
				+ [x] others
		- [x] Control.Moffy.Internal.SigとControl.Moffy.Internal.Sig.Commonについて
	+ [x] Moffy.Event -> Control.Moffy.Event
	+ [x] Moffy.XFieldHandle.Mouse
		- -> Control.Moffy.Handle.Mouse.XField or
		- [x] -> Control.Moffy.Handle.XField.Mouse
	+ [x] Moffy.EventHandle
		- -> Control.Moffy.Event
		- -> Control.Moffy.Handle
		- [x] Moffy.EventHandle.ThreadId
			* [x] -> Moffy.EventHandle.ThreadId.Event, Moffy.EventHandle.ThreadId.Handle
			* [x] Moffy.EventHandle.ThreadId -> Control.Moffy.Event, Control.Moffy.Handle
		- [x] Moffy.EventHandle.Lock
			* [x] -> Moffy.EventHandle.Lock.Event, Moffy.EventHandle.Lock.Handle
			* [x] -> Moffy.EventHandle.Lock -> Control.Moffy.Event, Control.Moffy.Handle
		- [x] Moffy.EventHandle.Random
			* [x] -> Moffy.EventHandle.Random.Event, Moffy.EventHandle.Random.Handle
			* [x] -> Moffy.EventHandle.Random -> Control.Moffy.Event, Control.Moffy.Handle
* [ ] API
* [ ] imports
* [ ] body

### Trials

### Moffy Base

ref
---

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```
task
----

* [x] qAppParで再帰的にタグをつけるのをやめる
	+ [x] 明示的にタグづけしたレベルだけ効率化する
* [x] タグづけした部分ではThreadIdはうまく働かないことを確かめる
	+ 問題は生じていない様子
