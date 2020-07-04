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
* [ ] 上記3つのモジュールを利用するようにする
	+ [x] Trial.CheckRandom
	+ [ ] Trial.Count
	+ [ ] Trial.CountWithLock
	+ [ ] Trial.TryThreadId
	+ [ ] Trial.TryCheckDup
	+ [ ] Trial.CheckSaring
	+ [ ] Trial.Boxes
	+ [ ] Trial.StepByStepBox
	+ [ ] Trial.Followbox

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
