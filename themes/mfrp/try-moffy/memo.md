memo
====

structure
---------

* Moffy
	+ Control.Moffy
		+ Event.ThreadId
		+ Handle
		+ Run
		+ Internal
			- Sig
			- Sig.Type
			- React
			- React.Type

* Moffy library
	+ Control.Moffy.Event
		- Lock
			* Internal
		- Random
			* Internal
		- Time
		- Delete
		- Key
			* Internal
			* Internal.XK
		- Mouse
	+ Control.Moffy.Handle
		- Lock
		- Random
		- Time
		- XField
			* Key
			* Mouse
* Moffy base
	+ Data.OneOrMore
	+ Data.Type.Set
		+ Internal
	+ Control.Monad.Freer.Par
		- Sequence
		- Funable
		- FTCQueue
		- TaggableFunction
		- Internal.Id
* Trial
	+ CheckRandom
	+ Count
	+ CountWithLock
	+ TryCheckDup
	+ TryKey
	+ TryLock
	+ TryThreadId
	+ CheckSharing
		- EvInt
		- ThreadId
	+ StepByStepBox
	+ Boxes
		- Event
		- Handle
	+ Followbox
		- Clickable
		- Event
		- HandleNew
		- View
		- Run
		- TypeSynonym

todo
----

* [ ] check module hierarchy
	+ [x] Moffy
	+ [ ] Moffy library
	+ [ ] Moffy base
	+ [ ] Trial
* [ ] refactoring
	+ [ ] Control.Moffy
	+ [ ] module structure of Control.Moffy.Event.Key
	+ [ ] Control.Moffy.Event.Key
	+ [ ] Trial
	+ [ ] Control.Monad.Freer.Par
	+ [ ] Data.OneOrMore
	+ [ ] Control.Moffy
		- [ ] consider rename adjust and adjustSig
			* [ ] adjust -> adjustReact ?
			* [ ] adjustSig -> adjust ?
		- [ ] others
	+ [ ] Data.Type.Set
		- [ ] numbered use fixed 64
		- [ ] others
	+ [ ] Control.Moffy.Handle
	+ [ ] Moffy
	+ [ ] Moffy library
	+ [ ] Moffy base
	+ [ ] Trials
	+ [ ] Control.Moffy.Internal.React.Type
	+ [ ] Control.Moffy.Handle
* [ ] move time event to Moffy library
* [ ] consider wheter or not to use MonadState
	+ [ ] refactor Control.Moffy.Handle.Lock
	+ [ ] others
* [ ] separate
	+ [ ] Trial
	+ [ ] Data.OneOrMore (with Data.Type.Set)
	+ [ ] Data.Type.Set or not
	+ [ ] Control.Monad.Freer
	+ [ ] Moffy library
		- [ ] Lock
		- [ ] Random
		- [ ] Delete, Key and Mouse
		- [ ] XField
* [ ] make tetris like game
	+ [ ] moffyris
* [ ] make handle using GTK
* [ ] consider whether to remove interpret and interpretReact
	+ [ ] remove interpret and interpretReact
	+ [ ] rename interpretSt and interpretReactSt
		to interpret and interpretReact
* [ ] consider whether to remove Handle and Handle'
	+ [ ] remove Handle and Handle'
	+ [ ] rename HandleSt and HandleSt' to Handle and Handle'
	+ [ ] consider whether to add (for example) function simple
		to make SimpleHandle


Moffy
-----

### module hierarchy

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

Control.Moffy.Event.ThreadId
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Handle.ThreadId
  +- Control.Moffy.Handle
  +- Control.Moffy.Event.ThreadId
```

### refactoring

* [x] Control.Moffy
	+ [x] API
		- [x] Type
			* [x] Sig
			* [x] ISig
			* [x] React
			* [x] Rct
			* [x] EvReqs
			* [x] EvOccs
			* [x] class Request
		- [x] Constraint
			* [x] Firstable
			* [x] Adjustable
		- [x] Combinator
			* [x] Await and Adjust
			* [x] Simple Sig
			* [x] Traverse
			* [x] Parallel
				+ [x] first
				+ [x] at
				+ [x] break
				+ [x] until
				+ [x] indexBy
			* [x] Copies
	+ [x] imports
* [x] Control.Moffy.Internal.Sig
	+ [x] API
		- [x] Adjust
		- [x] Parallel
			* [x] at
			* [x] break
			* [x] until
			* [x] indexBy
		- [x] Copies
			* [x] spawn
			* [x] parList
		- [x] Orphan instances
			* [x] Applicative (Flip (ISig s es) r)
			* [x] Applicative (Flip (Sig s es) r)
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FLIP APPLICATIVE
			* [x] INSTANCE
			* [x] APP AND IAPP
				+ [x] function app
				+ [x] function exposeBoth
				+ [x] function iapp
		- [x] PARALLEL
			* [x] AT
			* [x] BREAK AND UNTIL
			* [x] INDEX BY
		- [x] COPIES
			* [x] SPAWN
			* [x] PAR LIST
		- [x] BASIC
			* [x] ADJUST
			* [x] PAIRS
			* [x] PAUSE
* [ ] Control.Moffy.Internal.Sig.Type
	+ [x] API
		- [x] Type
			* [x] type Sig
			* [x] type ISig
			* [x] function isig
		- [x] Function
			* [x] Basic
				+ [x] emit and emitAll
				+ [x] waitFor
				+ [x] res and ires
				+ [x] hold
			* [x] Practical
				+ [x] repeat
				+ [x] find
				+ [x] scanl
	+ [x] imports
	+ [x] structure
	+ [ ] body
		- [x] TYPE
		- [ ] CLASS INSTANCE
			* [x] MONAD
				+ [x] Sig
				+ [x] ISig
			* [ ] FLIP FUNCTOR
		- [ ] FUNCTION
			* [ ] BASIC
			* [ ] PRACTICAL
* [ ] Control.Moffy.Internal.React
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Control.Moffy.Internal.React.Type
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Control.Moffy.Handle
* [ ] Control.Moffy.Run
* [ ] Control.Moffy.Event.ThreadId
* [ ] Control.Moffy.Handle.ThreadId

## Moffy library

### module hierarchy

```
Control.Moffy.Event
  + Control.Moffy.Event.Lock
      + Control.Moffy.Event.Lock.Internal
  + Control.Moffy.Event.Random
  + Control.Moffy.Event.Delete
  + Control.Moffy.Event.Key
      + Control.Moffy.Event.Key.Internal.XK
          + Control.Moffy.Event.Key.Internal
      + Control.Moffy.Event.Key.Internal
Control.Moffy.Handle
  + Control.Moffy.Handle.Lock
  + Control.Moffy.Handle.Random
  + Control.Moffy.Handle.XField
      + Control.Moffy.Handle.XField.Key
      + Control.Moffy.Handle.XField.Mouse
```

### refactor modules

Moffy Base
----------

### Control.Monad.Freer.Par

#### module hierarchy

```
Control.Monad.Freer.Par
  +- Control.Monad.Freer.Par.Sequence
  +- Control.Monad.Freer.Par.Funable
  |    +- Control.Monad.Freer.Par.Internal.Id
  +- Control.Monad.Freer.Par.Internal.Id

Control.Monad.Freer.Par.FTCQueue
  +- Control.Monad.Freer.Par.Sequence

Control.Monad.Freer.Par.TaggableFunction
  +- Control.Monad.Freer.Par.Funable
```

#### refactoring

### One Or More

#### module hierarchy

```
Data.OneOrMore
```

#### refactor

### Type Set

#### module hierarchy

```
Data.Type.Set
  +- Data.Type.Set.Internal
```

#### refactor

Trials
------

### tribial

#### Check Random

#### CheckSharing

#### CheckSharing.EvInt

#### CheckSharing.ThreadId

#### Count

#### Count With Lock

#### Try Check Dup

#### Try Key

#### Try ThreadId

### Boxes

### Followbox

ref
---

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```
task
----
