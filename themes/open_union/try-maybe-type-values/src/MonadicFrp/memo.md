memo
====

modules to expose
-----------------

### base

* MonadicFrp
* MonadicFrp.Handle
* MonadicFrp.Run

### library

* MonadicFrp.EventHandle.Thread
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
          + MonadicFrp.ThreadId.Type
  + MonadicFrp.React
      + MonadicFrp.React.Internal
      |   + MonadicFrp.ThreadId.Type
      + MonadicFrp.ThreadId.Type
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
* [ ] MonadicFrp.React.Internal
	+ [x] API
	+ [x] imports
	+ [x] structure
	+ [ ] body
		- [x] TYPE REACT
			* [x] TYPE DEFINITION
			* [x] MONAD
		- [ ] HANDLE
		- [ ] INTERPRET
		- [ ] COMBINATOR
			* [ ] TYPE SYNONYM
			* [ ] FUNCTION
* [ ] MonadicFrp.ThreadId.Type
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.Handle
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.Run
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.EventHandle.Thread
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.EventHandle.Lock
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.EventHandle.Random
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.Event.Mouse
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.XFieldHandle.Mouse
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
