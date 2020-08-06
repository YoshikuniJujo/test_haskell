memo
====

todo
----

* [x] rewrite interpretReact to use interpretReactSt
	+ [x] define function simple
		- `simple :: Handle m es -> HandleSt () () m es`
	+ [x] use function simple
* [x] rewrite interpret to use interpretSt
	+ [x] use function simple
* [x] define function liftHandle
* [ ] correct Handle, HandleSt and so on
	+ Handle, Handle', HandleSt, HandleSt', HandleIo, HandleIo'
	+ [x] rename HandleSt' to HandleIo'
	+ [x] define HandleSt'
		- `type HandleSt' st m es = HandleIo' st st m es`
	+ [x] rename functions about HandleIo'
	* [x] define function expandSt
	* [ ] define function foo and bar
		+ [ ] create name for function foo and bar
		+ [ ] foo :: (a -> HandleSt' st m es) -> HandleIo' (a, st) st m es
		+ [ ] bar :: HandleIo' (a, st) st m es -> a -> HandleSt' st m es
	+ [ ] define HandleIo and redefine HandleSt using HandleIo
		- `type HandleSt st m es = HandleIo st st m es`
	+ [ ] others
* [ ] consider whether to remove interpret and interpretReact
	+ [ ] remove interpret and interpretReact
	+ [ ] rename interpretSt and interpretReactSt
		to interpret and interpretReact
* [ ] consider whether to remove Handle and Handle'
	+ [ ] remove Handle and Handle'
	+ [ ] rename HandleSt and HandleSt' to Handle and Handle'
	+ [ ] consider whether to add (for example) function simple
		to make SimpleHandle
* [ ] consider whether or not to change
	+ from HandleSt' s s m es
	+ to HandleSt' s m es
* [ ] check module hierarchy
	+ [ ] Moffy
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

Control.Moffy.Event.ThreadId
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Handle.ThreadId
  +- Control.Moffy.Handle
  +- Control.Moffy.Event.ThreadId
```

#### todo

* [ ] refactoring

#### refactoring

* [x] Control.Moffy
	+ [x] API
		- [x] adjustSig
			* [x] structure
			* [x] rename or not
		- [x] others
			* [x] Type
				+ [x] Sig
				+ [x] ISig
				+ [x] React
				+ [x] Rct
				+ [x] EvReqs
				+ [x] EvOccs
				+ [x] Request
			* [x] Constraint
			* [x] Combinator
				+ [x] Await and Adjust
				+ [x] structure
				+ [x] Conversion
					- [x] rename Conversion to Simple Sig
				+ [x] Simple Sig
				+ [x] Traverse
				+ [x] Parallel
				+ [x] Copies
	+ [x] imports
* [x] Control.Moffy.Internal.Sig
	+ [x] API
		- [x] about adjustSig
		- [x] structure
		- [x] Adjust
		- [x] Parallel
		- [x] Copies
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FLIP APPLICATIVE
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
* [x] Control.Moffy.Internal.Sig.Type
	+ [x] API
		- [x] Type
		- [x] Function
			* [x] Basic
			* [x] Practical
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] TYPE
		- [x] CLASS INSTANCE
			* [x] MONAD
			* [x] FLIP FUNCTOR
		- [x] FUNCTION
			* [x] BASIC
			* [x] PRACTICAL
* [x] Control.Moffy.Internal.React
	+ [x] API
		- [x] structure
		- [x] Class
		- [x] Constraint
		- [x] Function
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] ADJUST
		- [x] FIRST
		- [x] PAR
		- [x] UPDATE
* [x] Control.Moffy.Internal.React.Type
	+ [x] API
		- [x] structure
		- [x] React
			* [x] Type
			* [x] Never and Await
		- [x] Handle
		- [x] ThreadId
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] REACT
			* [x] TYPE
			* [x] NEVER AND AWAIT
		- [x] HANDLE
		- [x] THREAD ID
* [x] Control.Moffy.Handle
	+ [x] API
		- [x] structure
		- [x] Constraint
		- [x] Composer
			* [x] Plain
			* [x] With State
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] CONSTRAINT
		- [x] PLAIN
		- [x] WITH STATE
* [x] Control.Moffy.Run
	+ [x] API
		- [x] consider whether or not to return st in interpretSt
		- [x] Type
		- [x] Run
			* [x] st -> Foo -> m (r, st) ===> Foo -> st -> m (r, st)
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] SIG
		- [x] REACT
* [x] Control.Moffy.Internal.Sig
	+ [x] correct function at
		- [x] define iat
		- [x] use iat in at
* [x] Control.Moffy.Event.ThreadId
	+ [x] API
	+ [x] imports
	+ [x] body
* [x] Control.Moffy.Handle.ThreadId
	+ [x] API
	+ [x] imports
	+ [x] body

### Moffy library

#### module hierarchy

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

#### refactor modules

* [x] Control.Moffy.Event.Lock
	+ [x] API
	+ [x] imports
* [x] Control.Moffy.Event.Lock.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] LOCK ID
		- [x] EVENT
			* [x] NEW LOCK ID
			* [x] GET LOCK
			* [x] UNLOCK
		- [x] WITH LOCK
* [x] Control.Moffy.Handle.Lock
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
* [x] separate Control.Moffy.Event.Random to ....Internal
* [x] Control.Moffy.Event.Random
	+ [x] API
	+ [x] imports
* [x] Control.Moffy.Event.Random.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] EVENT
			* [x] STORE RANDOM GEN
			* [x] LOAD RANDOM GEN
		- [x] RANDOM EV AND GET RANDOM
* [x] Control.Moffy.Handle.Random
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
* [ ] separate time event from boxes
	+ [x] separate Trial.Boxes.Handle.TimeEv from Trial.Boxes.Handle
	+ [x] moddify Trial.Boxes.Handle.TimeEv
		- [x] use m (TaiMonad m) instead of IO
		- [x] handleTimeEvPlus: consider to return HandleSt' instead of HandleSt
		- [x] handleBoxes: use AbsoluteTimeState instead of StateT
			* [x] make handleBoxes'
			* [x] replace handleBoxes by handleBoxes'
				+ [x] replace about tryBoxes
				+ [x] others
			* [x] remove handleBoxes
				+ [x] move to where clause
				+ [x] remove
			* [x] rename handleBoxes' to handleBoxes
		- [x] handleTimeEvPlus: use HandleSt' instead of StateT
		- [x] rename handleTimeEvPlus' to handleTimeEvPlus
		- [x] handleTimeEvPlus: use s instead of (Mode, AbsoluteTime)
		- [x] handleTimeEvPlus: use HandleSt' instead of Handle' as argument
		- [x] handleTimeEvPlus: try using HandleSt' instead of `DiffTime -> a -> `
		- [x] handleTimeEvPlus: add state s to argument function of handleTimeEvPlus
			* [x] step 1
			* [x] others
		- [x] use HandleSt'
			* `ModeState s =>`
				`(DiffTime -> a -> HandleSt' s s m) -> Difftime ->`
				`a -> HandleSt' s s m (es :+: TimeEv)`
		- [x] separate TimeEv from Trial.Boxes.Event
	+ [x] move TimeEv to Control.Moffy.Event.Time
		- [x] separate TimeEv from Trial.Boxes.Event
			* [x] tribial fix for haddock bug
		- [x] move Trial.Boxes.Event.Time to Control.Moffy.Event.Time
	+ [x] move Trial.Boxes.Handle.TimeEv to Control.Moffy.Handle.Time
	+ [ ] refactor Control.Moffy.Handle.Time
	+ [ ] refactor Trial.Boxes.Event
	+ [ ] others
* [ ] Control.Moffy.Event.Delete
* [ ] Control.Moffy.Event.Key
* [ ] Control.Moffy.Event.Key.Internal
* [ ] Control.Moffy.Event.Key.Internal.XK
* [ ] Control.Moffy.Handle.XField
* [ ] Control.Moffy.Handle.XField.Key
* [ ] Control.Moffy.Handle.XField.Mouse

### Moffy Base

#### Control.Monad.Freer.Par

##### module hierarchy

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

##### refactoring

#### One Or More

##### module hierarchy

```
Data.OneOrMore
```

##### refactor

#### Type Set

##### module hierarchy

```
Data.Type.Set
  +- Data.Type.Set.Internal
```

##### refactor

### Trials

#### tribial

##### Check Random

##### CheckSharing

##### CheckSharing.EvInt

##### CheckSharing.ThreadId

##### Count

##### Count With Lock

##### Try Check Dup

##### Try Key

##### Try ThreadId

#### Boxes

#### Followbox

ref
---

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```
task
----
