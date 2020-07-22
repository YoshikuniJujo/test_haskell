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
* [x] refactoring
	+ [x] Control.Monad.Freer.Par
	+ [x] Data.OneOrMore
	+ [x] Data.Type.Set
* [x] use pattern synonym Singleton
	+ [x] define pattern Singleton
	+ [x] remove function singleton and extract
	+ [x] correct other modules
* [x] try using key events of X11
	+ [x] Control.Moffy.Event.Key
		- [x] pattern synonym to ASCII
		- [x] key to char ASCII
			* use PatternSynonyms and ViewPatterns
				+ pattern AsciiKey :: Char -> Key
		- [x] pattern synonym to HHKB key other than ASCII
			* [x] make keysym file
		- [x] Key Events
			* [x] Key Press Event
			* [x] Key Release Event
	+ [x] Control.Moffy.Handle.XField.Key
		- [x] handle Key Press Event
			* [x] check shift by ShiftMask
		- [x] handle Key Release Event
* [ ] refactoring
	+ [ ] Control.Moffy
	+ [ ] module structure of Control.Moffy.Event.Key
	+ [ ] Control.Moffy.Event.Key
	+ [ ] Trial
	+ [ ] Control.Monad.Freer.Par
	+ [ ] Data.OneOrMore

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
* [x] Control.Moffy.Handle
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] PLAIN
		- [x] WITH STATE
			* [x] HandleSt': flip st and (EvReqs es)
			* [x] HandleSt: flip st and (EvReqs es)
			* [x] others
* [x] Control.Moffy.Run
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] RUN SIG
		- [x] RUN REACT

### Moffy library

#### module hierarchy

```
Control.Moffy.Event
  + Control.Moffy.Event.ThreadId
  + Control.Moffy.Event.Lock
      + Control.Moffy.Event.Lock.Internal
  + Control.Moffy.Event.Random
  + Control.Moffy.Event.Delete
  + Control.Moffy.Event.Key
      + Control.Moffy.Event.Key.XK
      + Control.Moffy.Event.Key.Internal
Control.Moffy.Handle
  + Control.Moffy.Handle.ThreadId
  + Control.Moffy.Handle.Lock
  + Control.Moffy.Handle.Random
  + Control.Moffy.Handle.XField
      + Control.Moffy.Handle.XField.Key
      + Control.Moffy.Handle.XField.Mouse
```

#### module dependency hierarchy

```
Control.Moffy.Event.Key
  + Control.Moffy.Event.Key.XK
      + Control.Moffy.Event.Key.Internal
  + Control.Moffy.Event.Key.Internal
```

#### refactor modules

* [x] Control.Moffy.Event.ThreadId
* [x] Control.Moffy.Handle.ThreadId
* [x] Control.Moffy.Event.Lock
	+ [x] API
	+ [x] imports
* [x] Control.Moffy.Event.Lock.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] LOCKSTATE AND LOCKID
		- [x] EVENT
			* [x] NEW LOCK ID
			* [x] GET LOCK
			* [x] UNLOCK
		- [x] WITHLOCK
* [x] Control.Moffy.Handle.Lock
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		+ [x] LOCK STATE
		+ [x] HANDLE
* [x] Control.Moffy.Event.Random
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] EVENT
			* [x] STORE RANDOM GEN
			* [x] LOAD RANDOM GEN
		- [x] REACT
			* [x] TYPE
			* [x] GET RANDOM FUNCTION
* [x] Control.Moffy.Handle.Random
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] RANDOM STATE
		- [x] HANDLE
* [x] Control.Moffy.Event.Mouse
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] MOUSE DOWN
		- [x] MOUSE UP
		- [x] MOUSE MOVE
		- [x] DELETE EVENT
		- [x] MOUSE EV
* [x] Control.Moffy.Event.Key
	+ [x] module hierarchy
		- [x] rename or not Control.Moffy.Event.Key.XK
			* [x] move it under Control.Moffy.Event.Key.Internal
			* [x] rename or not it
		- [x] rename or not Control.Moffy.Event.Key.Internal
		- [x] API of Control.Moffy.Event.Key.Internal.XK
		- [x] API of Control.Moffy.Event.Key.Internal
* [x] Control.Moffy.Event.Key.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] TYPA AND AUXILIARY FUNCTION
		- [x] DEFINITION OF PATTERN XK_FOO
			* [x] VOID SYMBOL
			* [x] TTY FUNCTION KEYS
			* [x] JAPANESE KEYBOARD SUPPORT
			* [x] CURSOR CONTROL AND MOTION
			* [x] MISC FUNCTION
			* [x] AUXILIARY FUNCTIONS
			* [x] MODIFIERS
			* [x] KEYBOARD (XKB) EXTENSION FUNCTION AND MODIFIER KEYS
			* [x] LATIN 1 (only ASCII)
* [ ] Control.Moffy.Event.Key.Internal.XK
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Control.Moffy.Event.Key
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Control.Moffy.Handle.XField.Key
* [ ] Control.Moffy.Handle.XField.Mouse
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body

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
