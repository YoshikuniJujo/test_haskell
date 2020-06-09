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
* [ ] MonadicFrp.Handle
* [ ] MonadicFrp.Run

### library

* [ ] MonadicFrp.EventHandle
	+ [ ] Thread
	+ [ ] Lock
	+ [ ] Random
* [ ] Mouse
	+ [ ] MonadicFrp.Event.Mouse
	+ [ ] MonadicFrp.XFieldHandle.Mouse

refactor
--------

* [ ] MonadicFrp
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.Sig
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.React
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
* [ ] MonadicFrp.React.Internal
	+ [ ] API
	+ [ ] imports
	+ [ ] structure
	+ [ ] body
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
