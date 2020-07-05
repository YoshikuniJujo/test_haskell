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
  + Control.Moffy.Internal.Sig
  |   + Control.Moffy.Internal.Sig.Common
  |   |   + Control.Moffy.Internal.React.Common
  |   + Control.Moffy.Internal.React
  |   |   + Control.Moffy.Internal.React.Common
  |   + Control.Moffy.Internal.React.Common
  + Control.Moffy.Internal.Sig.Common
  + Control.Moffy.Internal.React
  + Control.Moffy.Internal.React.Common

Control.Moffy.Handle
  + Control.Moffy.Internal.Handle
  |   + Control.Moffy.Internal.React.Common
  + Control.Moffy.Internal.React.Comon

Control.Moffy.Run
  + Control.Moffy.Internal.Sig.Common
  + Control.Moffy.Internal.React
  + Control.Moffy.Internal.React.Common
```

#### todo

* [ ] モジュールの整理
	+ [x] 現在のモジュール構造のチェック
	+ [ ] モジュールの整理
		- [ ] Moffy.ReactとMoffy.React.Commonについて
			* [x] add export list
			* [ ] othres
		- [x] Control.Moffy.Internal.Fooに移動
		- [ ] Control.Moffy.Internal.ReactとControl.Moffy.Internal.React.Commonについて
			* [ ] Type, Handle, Runをきれいに分ける
				+ [ ] Control.Moffy.Internal..Handle -> Control.Moffy.Handle
				+ [ ] others
		- [ ] Control.Moffy.Internal.SigとControl.Moffy.Internal.Sig.Commonについて
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
