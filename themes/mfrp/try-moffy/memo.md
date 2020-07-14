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
* [x] solve problem about Id and Taggable
	+ [x] rename Control.Monad.Freer.Par.Fun to ...Funable
	+ [x] rename Fun to Funable
	+ [x] others
* [ ] refactoring
	+ [x] Control.Monad.Freer.Par
	+ [x] Data.OneOrMore
	+ [x] Data.Type.Set
	+ [ ] Control.Moffy
	+ [ ] Trial
	+ [ ] Control.Monad.Freer.Par

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

* [x] モジュールの整理
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
* [ ] refactoring

#### refactoring

* [x] Control.Moffy
	+ [x] API
	+ [x] imports
* [x] Control.Moffy.Internal.Sig
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FLIP APPLICATIVE
		- [x] REPETITIVE COMBINATOR
			* [x] SPAWN
			* [x] PAR LIST
		- [x] PARALLEL COMBINATOR
			* [x] AT
			* [x] BRAEK AND UNTIL
			* [x] INDEX BY
		- [x] BASIC COMBINATOR
			* [x] PAUSE
			* [x] PAIRS
			* [x] ADJUST
* [x] Control.Moffy.Internal.Sig.Type
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] TYPE
		- [x] TYPE CLASS INSTANCE
			* [x] MONAD
			* [x] FLIOP FUNCTOR
		- [x] FUNCTION
			* [x] BASIC
			* [x] PRACTICAL
* [x] Control.Moffy.Internal.React
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] ADJUST
		- [x] FIRST
		- [x] PAR
		- [x] UPDATE
* [x] Control.Moffy.Internal.React.Type
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] REACT AND HANDLE
		- [x] THREAD ID
		- [x] AWAIT AND NEVER
* [ ] Control.Moffy.Handle
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [ ] body
		- [x] PLAIN
		- [ ] WITH STATE
			* [x] HandleSt': flip st and (EvReqs es)
			* [x] HandleSt: flip st and (EvReqs es)
			* [ ] others
* [ ] Control.Moffy.Run

### Moffy library

* [ ] Control.Moffy.Event.ThreadId
* [ ] Control.Moffy.Handle.ThreadId
* [ ] Control.Moffy.Event.Lock
* [ ] Control.Moffy.Handle.Lock
* [ ] Control.Moffy.Event.Random
* [ ] Control.Moffy.Handle.Random
* [ ] Control.Moffy.Event.Mouse
* [ ] Control.Moffy.Handle.XField.Mouse

### Moffy Base

#### Control.Monad.Freer.Par

##### module hierarchy

* [x] Freer -> Freer, Fun
* [x] Freer -> Control.Monad.Freer.Par
* [x] Sequence -> Control.Monad.Freer.Par.Sequence
* [x] Fun -> Control.Monad.Freer.Par.Fun
* [x] Control.Monad.Freer.Par.FTCQueue
* [x] Control.Monad.Freer.Par.TaggableFunction

```
Control.Monad.Freer.Par
  +- Control.Monad.Freer.Par.Sequence
  +- Control.Monad.Freer.Par.Fun

Control.Monad.Freer.Par.FTCQueue
  +- Control.Monad.Freer.Par.Sequence

Control.Monad.Freer.Par.TaggableFunction
  +- Control.Monad.Freer.Par.Fun
```

##### refactoring

* [x] Control.Monad.Freer.Par.Sequence
	+ [x] API
	+ [x] body
* [x] Control.Monad.Freer.Par.Fun
	+ [x] API
	+ [x] body
* [x] Control.Monad.Freer.Par
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] PARALLEL FREER
			* [x] TYPE AND MONAD
			* [x] APPLICATION
		- [x] UNIQUE ID
* [x] Control.Monad.Freer.Par.FTCQueue
	+ [x] API
	+ [x] imports
	+ [x] body
* [x] Control.Monad.Freer.Par.TaggableFunction
	+ [x] API
	+ [x] imports
	+ [x] body

#### One Or More

##### module hierarchy

```
Data.OneOrMore
```

##### refactor

* [x] Data.OneOrMore
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] ONEORMORE TYPE
		- [x] BASIC PROPERTY
			* [x] PROJECTABLE
			* [x] INSERTABLE
		- [x] EXPANDABLE AND COLLAPSABLE
			* [x] EXPANDABLE
			* [x] COLLAPSABLE
				+ [x] Collapsable0
				+ [x] Collapsable
		- [x] MERGEABLE

#### Type Set

##### module hierarchy

```
Data.Type.Set
  +- Data.Type.Set.Internal
```

##### refactor

* [x] Data.Type.Set
	+ [x] API
	+ [x] imports
* [x] Data.Type.Set.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body

### Trials

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
