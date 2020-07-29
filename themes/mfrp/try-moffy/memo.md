memo
====

todo
----

* [ ] check module hierarchy
	+ [x] Moffy
	+ [ ] Moffy library
	+ [ ] Moffy base
* [ ] refactoring
	+ [x] package simple-field
		- [x] about Event'
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
* [ ] Control.Moffy.Handle
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] Control.Moffy.Run
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body

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
      + Control.Moffy.Event.Key.Internal.XK
          + Control.Moffy.Event.Key.Internal
      + Control.Moffy.Event.Key.Internal
Control.Moffy.Handle
  + Control.Moffy.Handle.ThreadId
  + Control.Moffy.Handle.Lock
  + Control.Moffy.Handle.Random
  + Control.Moffy.Handle.XField
      + Control.Moffy.Handle.XField.Key
      + Control.Moffy.Handle.XField.Mouse
```

#### refactor modules

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
