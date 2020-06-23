memo
====

modules to expose
-----------------

### base

* MonadicFrp
* MonadicFrp.Handle
* MonadicFrp.Run

### library

* MonadicFrp.EventHandle.ThreadId
* MonadicFrp.EventHandle.Lock
* MonadicFrp.EventHandle.Random
* MonadicFrp.Event.Mouse
* MonadicFrp.XFieldHandle.Mouse

module hierarchy
----------------

```
MonadicFrp
  + MonadicFrp.Sig
  |   + MonadicFrp.React.Internal
  + MonadicFrp.React
      + MonadicFrp.React.Internal
```

```
MonadicFrp.Handle
  + MonadicFrp.React
```

```
MonadicFrp.Run
  + MonadicFrp.Sig
  + MOnadicFrp.React
```

refactor API to expose
----------------------

### base

* [x] MonadicFrp
	+ [x] Types
	+ [x] React
	+ [x] Conversion
	+ [x] Transformation
	+ [x] Repetition
	+ [x] Parallel composition
* [x] MonadicFrp.Handle
	+ [x] Types
	+ [x] Composer
* [x] MonadicFrp.Run
	+ [x] Types
	+ [x] Run

### library

* [x] MonadicFrp.EventHandle
	+ [x] Thread
	+ [x] Lock
	+ [x] Random
* [x] Mouse
	+ [x] MonadicFrp.Event.Mouse
	+ [x] MonadicFrp.XFieldHandle.Mouse

refactor
--------

* [x] MonadicFrp
	+ [x] imports
* [x] MonadicFrp.Sig
	+ [x] API
	+ [x] imports
	+ [x] structure
		- [x] 1st
		- [x] instance Applicative (Flip (ISig es) r)
		- [x] 2nd
	+ [x] body
		- [x] TYPE SIG AND ISIG
			* [x] general
			* [x] HOLD, PAIRS AND PAUSE
			* [x] MONAD
			* [x] FLIP APPLICATIVE
		- [x] INTERPRET
			* [x] 1st
			* [x] 2nd
		- [x] COMBINATOR
			* [x] CONVERSION
			* [x] TRANSFORMATION
			* [x] REPETITION
			* [x] PARALLEL COMPOSITION
				+ [x] at
				+ [x] break
				+ [x] until
				+ [x] indexBy
* [x] MonadicFrp.React
	+ [x] API
	+ [x] imports
	+ [x] body
* [x] MonadicFrp.React.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] TYPE REACT
			* [x] TYPE DEFINITION
				+ [x] ThreadId
			* [x] MONAD
		- [x] HANDLE
		- [x] INTERPRET
		- [x] COMBINATOR
			* [x] CONTEXT SYNONYM
			* [x] FUNCTION
				+ [x] adjust
				+ [x] first
				+ [x] par
* [x] MonadicFrp.Handle
	+ [x] imports
	* [x] rename expandHandleSt and mergeHandleSt
		+ to expandSt, mergeSt
	* [x] make beforeSt
	* [x] define fixity to expandSt, beforeSt and mergeSt
	* [x] API
	+ [x] structure
	+ [x] body
		- [x] CONSTRAINT SYNONYM
		- [x] HANDLE WITH NO STATE
		- [x] HANDLE WITH STATE
* [x] MonadicFrp.Run
	+ [x] imports
* [x] MonadicFrp.EventHandle.ThreadId
	+ [x] API
	+ [x] imports
	+ [x] body
* [x] MonadicFrp.EventHandle.Lock
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] LOCKID AND LOCKSTATE
		- [x] EVENT
			* [x] NEWLOCKID
			* [x] GETLOCK
			* [x] UNLOCK
		- [x] HANDLE AND WITHLOCK
* [x] MonadicFrp.EventHandle.Random
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] EVENT
			* [x] STORE RANDOM GEN
			* [x] LOAD RANDOM GEN
		- [x] REACT AND HANDLE
			* [x] TYPE
			* [x] GET RANDOM FUNCTION
			* [x] HANDLE
* [x] MonadicFrp.Event.Mouse
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] MOUSE DOWN
		- [x] MOUSE UP
		- [x] MOUSE MOVE
		- [x] DELETE EVENT
* [ ] MonadicFrp.XFieldHandle.Mouse
	+ [x] API
	+ [x] imports
	+ [ ] structure
	+ [ ] body

TODO
----

* [ ] add function to test
	+ [ ] foo :: React es a -> (EvReqs es -> EvOccs es) -> React es a
		- [ ] foo' :: React es a -> (EvReqs es -> m (EvOccs es)) -> m (React es a)
		- [ ] getFoo :: React es a -> EvReqs es
		- [ ] putFoo :: React es a -> EvOccs es -> React es a
	+ [ ] bar :: Sig es a r -> (EvReqs es -> EvOccs es) -> (Maybe a, Sig es a r)
		- [ ] bar' :: Sig es a r -> (EvReqs es -> m (EvOccs es)) -> m (Maybe a, Sig es a r)
		- [ ] getBar :: Sig es a r -> EvReqs es
		- [ ] putBar :: Sig es a r -> EvOccs es -> (Maybe a, Sig es a r)
