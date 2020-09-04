memo
====

structure
---------

* Moffy (10)
	+ Control.Moffy
		+ NoThreadId
		+ Handle
		+ Run
		+ Internal
			- Sig
			- Sig.Type
			- React
			- React.Type
		+ Event.ThreadId
		+ Handle.ThreadId
* Moffy library (16)
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
* Moffy base (9)
	+ Data.OneOrMore (1)
	+ Data.Type.Set (2)
		+ Internal
	+ Control.Monad.Freer.Par (6)
		- Sequence
		- Funable
		- FTCQueue
		- TaggableFunction
		- Internal.Id
* Trial (20)
	+ CheckRandom
	+ Count
	+ CountWithLock
	+ TryCheckDup
	+ TryKey
	+ TryLock
	+ TryThreadId
	+ TrySharing
	+ CheckSharing.EvInt
	+ CheckSharing.TrheadId
	+ StepByStepBox
	+ Boxes
		- Event
		- Handle
	+ Followbox
		- Clickable
		- Event
		- Handle
		- View
		- ViewType
		- Run
		- TypeSynonym

todo
----

* [x] repair Control.Moffy.Internal.React.first
	+ [x] rename AdjustableAdjustable
	+ [x] remove first
	+ [x] rename first' to first
* [x] refactor Control.Moffy.Internal.React
* [x] refactor Control.Moffy.Internal.Sig
* [x] refactor Control.Moffy.Internal.React.Type
	+ [x] move constraint synonym FooOccurred to here
* [x] refactor Control.Moffy.Handle
* [x] separate WithThreadId and WithNoThreadId
	+ [x] add NoThreadId and noThreadId
	+ [x] correct module Control.Moffy.Internal.React and Control.Moffy.Internal.Sig
		- [x] define functions
			* first_, at_, break_, until_, indexBy_, parList_
			* `foo_ :: React s es (ThreadId, ThreadId) -> ...`
	+ [x] correct module Control.Moffy
		- [x] move definition of first, at, break, until, indexBy, parList from Internal.React and Internal.Sig
			* [x] first
			* [x] at
			* [x] break
			* [x] until
			* [x] indexBy
			* [x] parList
	+ [x] make module Control.Moffy.NoThreadId
		- [x] define functions
			* [x] app'
			* [x] iapp'
			* [x] first'
			* [x] at'
			* [x] break' and until'
			* [x] indexBy'
			* [x] parList'
* [x] try with Trial.TrySharing
* [x] refactor Control.Moffy
* [x] refactor Control.Moffy.NoThreadId
* [x] refactor Control.Moffy.Internal.Sig
* [x] refactor Control.Moffy.Internal.React
* [x] refactor Control.Moffy.Internal.React.Type
* [x] refactor Control.Moffy.Handle
* [ ] make handle using GTK
	+ [x] consider to use TChan in Handle.TChan
	+ [x] handle Delete Event
	+ [x] handle Mouse Event
		- [x] MotionNotifyEvent
			* [x] make MotionNotifyEvent
			* [x] add gtk_widget_set_events
		- [x] ButtonPressEvent and ButtoReleaseEvent
			* [x] extract button
			* [x] extract x and y
			* [x] ButtonPressEvent
			* [x] ButtonReleaseEvent
	+ [x] handle Key Event
		- [x] KeyPressEvent and KeyReleaseEvent
			* [x] KeyPressEvent
			* [x] KeyReleaseEvent
	+ [x] remove Control.Moffy.Handle.TimeTChan
	+ [x] draw rectangle
		- [x] draw contour
		- [x] set color
		- [x] fill rectangle
	+ [x] free Mutable
	+ [ ] arrange
	+ [ ] test followbox
		- [x] draw character
		- [ ] draw image
			* [x] try to use cairo_image_surface_create_from_png from C
			* [x] try to use cairo_image_surface_create_from_png from Haskell
			* [ ] try to use cairo_image_surface_create_from_png_stream from C
			* [ ] try to use cairo_image_surface_create_from_png_stream from Haskell
		- [ ] investigate about XGlyphInfo
			* cairo_text_extents_t
			* cairo_text_extents
		- [ ] try followbox
	+ [ ] use ForeignPtr to free memory for cairo surface
	+ [ ] refactoring
	+ [ ] others
* [ ] about window
	+ [ ] make window
	+ [ ] make multiple window
	+ [ ] window move event
	+ [ ] make tear-off sample
* [ ] check module hierarchy
	+ [ ] Moffy
	+ [x] Moffy library
	+ [ ] Moffy base
	+ [ ] Trial
* [ ] refactoring
	+ [x] Control.Moffy
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
	+ [x] Moffy
	+ [x] Moffy library
	+ [ ] Moffy base
* [ ] move time event to Moffy library
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
* [ ] consider whether to remove interpret and interpretReact
	+ [ ] remove interpret and interpretReact
	+ [ ] rename interpretSt and interpretReactSt
		to interpret and interpretReact
* [ ] consider whether to remove Handle and Handle'
	+ [ ] remove Handle and Handle'
	+ [ ] rename HandleSt and HandleSt' to Handle and Handle'
	+ [ ] consider whether to add (for example) function simple
		to make SimpleHandle
* [ ] consider whether or not to put `deriving Show' to Occurred Foo
* [ ] define Data.Or.or


Moffy (9)
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

Control.Moffy.NoThreadId
  +- Control.Moffy.Internal.Sig
  +- Control.Moffy.Internal.Sig.Type
  +- Control.Moffy.Internal.React
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
		- [x] structure
		- [x] Sig
			* [x] Sig
			* [x] ISig
		- [x] React
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
				+ [x] await
				+ [x] adjust
				+ [x] adjustSig
			* [x] Create Sig
				+ [x] emit
				+ [x] waitFor
				+ [x] repeat
			* [x] Traverse
				+ [x] find
				+ [x] scanl
			* [x] Parallel
				+ [x] first
				+ [x] at
				+ [x] break
				+ [x] until
				+ [x] indexBy
			* [x] Copies
				+ [x] spawn
				+ [x] parList
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] PARALLEL
			* [x] function first
			* [x] function at
			* [x] function break
			* [x] function until
			* [x] function indexBy
		- [x] COPIES
			* [x] function parList
* [x] Control.Moffy.NoThreadId
	+ [x] API
		- [x] structure
		- [x] Applicative
			* [x] function app'
			* [x] function iapp'
		- [x] Parallel
			* [x] function first'
			* [x] function at'
			* [x] function break'
			* [x] function until'
			* [x] indexBy'
		- [x] Copies
			* [x] parList'
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] APPLICATIVE
			* [x] function app'
			* [x] function iapp'
		- [x] PARALLEL
			* [x] function first'
			* [x] function at'
			* [x] function break' and until'
			* [x] function indexBy'
		- [x] COPIES
			* [x] function parList'
* [x] Control.Moffy.Internal.Sig
	+ [x] API
		- [x] structure
		- [x] Adjust
		- [x] Applicative
			* [x] app_
			* [x] iapp_
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
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FLIP APPLICATIVE
			* [x] INSTANCE
				+ [x] instance Applicative (Flip (Sig s es ) r)
				+ [x] instance Applicative (Flip (ISig s es) r)
			* [x] APP AND IAPP
				+ [x] function app_
				+ [x] function exposeBoth_
				+ [x] function iapp_
		- [x] PARALLEL
			* [x] AT
				+ [x] function at_
				+ [x] function iat_
			* [x] BREAK AND UNTIL
				+ [x] function break_
				+ [x] function until_
			* [x] INDEX BY
				+ [x] function indexBy_
				+ [x] function indexByGen_
				+ [x] function iiindexBy_
		- [x] COPIES
			* [x] SPAWN
			* [x] PAR LIST
				+ [x] function parList_
				+ [x] function iparList_
				+ [x] function cons_
		- [x] BASIC COMBINATOR
			* [x] ADJUST
				+ [x] function adjustSig
				+ [x] function adjustISig
			* [x] PAIRS
				+ [x] function ipairs_
			* [x] PAUSE
				+ [x] function pause_
				+ [x] function ipause_
* [x] Control.Moffy.Internal.Sig.Type
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
	+ [x] body
		- [x] TYPE
		- [x] CLASS INSTANCE
			* [x] MONAD
				+ [x] Sig
				+ [x] ISig
			* [x] FLIP FUNCTOR
		- [x] FUNCTION
			* [x] BASIC
			* [x] PRACTICAL
* [x] Control.Moffy.Internal.React
	+ [x] API
		- [x] structure
		- [x] Class
			* [x] Adjustable
			* [x] Updatetable
		- [x] Constraint Synonym
			* [x] Firstable
		- [x] Function
			* [x] first_
			* [x] adjust
			* [x] par_
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FIRST
			* [x] constraint synonym Firstable
			* [x] function first_
		- [x] ADJUST
			* [x] structure
			* [x] class Adjustable
			* [x] instance Adjustable
			* [x] function adj
		- [x] PAR
		- [x] UPDATABLE
			* [x] class Updatable
			* [x] instance Updatable a a
			* [x] instance Updatable a b
* [x] Control.Moffy.Internal.React.Type
	+ [x] API
		- [x] structure
		- [x] React
			* [x] Type React and Data Rct
				+ [x] type React
				+ [x] data Rct
				+ [x] type EvReqs
				+ [x] type EvOccs
			* [x] Class Request
			* [x] Constraint Synonym for Data Occurred
				+ [x] ExpandableOccurred
				+ [x] CollapsableOccurred
				+ [x] MergeableOccurred
		- [x] Never and Await
			* [x] never
			* [x] await
			* [x] await'
		- [x] Handle
			* [x] type Handle
			* [x] type HandleSt
			* [x] type St
			* [x] function liftHandle
			* [x] function liftSt
		- [x] ThreadId
			* [x] data ThreadId
			* [x] value rootThreadId
			* [x] react noForkThreadId
			* [x] react forkThreadId
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] REACT
			* [x] TYPE
				+ [x] type React
				+ [x] data Rct
				+ [x] class Request
				+ [x] type EvReqs
				+ [x] type EvOccs
			* [x] NEVER AND AWAIT
				+ [x] react never
				+ [x] function await
				+ [x] function await'
		- [x] CONSTRAINT SYNONYM
			* [x] ExpandableOccurred
			* [x] CollapsableOccurred
			* [x] MergeableOccurred
		- [x] HANDLE
			* [x] type Handle
			* [x] type HandleSt
			* [x] type St
			* [x] funciton liftHandle
			* [x] function liftSt
		- [x] THREAD ID
			* [x] data ThreadId
			* [x] value rootThreadId
			* [x] react noForkThreadId
			* [x] react forkThreadId
* [x] Control.Moffy.Handle
	+ [x] API
		- [x] structure
		- [x] Constraint
			* [x] ExpandableHandle
			* [x] ExpandableOccurred
			* [x] MergeableOccurred
		- [x] Handle and Function
			* [x] Plain
				+ [x] Type
					- [x] Handle
					- [x] Handle'
				+ [x] Composer
					- [x] retry
					- [x] expand
					- [x] before
					- [x] merge
			* [x] With State
				+ [x] Type
					- [x] HandleSt
					- [x] HandleSt'
					- [x] St
					- [x] liftHandle
					- [x] liftHandle'
					- [x] liftSt
				+ [x] Composer
					- [x] retrySt
					- [x] expandSt
					- [x] beforeSt
					- [x] mergeSt
			* [x] With Input and Output
				+ [x] Type
					- [x] HelloIo'
					- [x] pushInput
					- [x] popInput
				+ [x] Composer
					- [x] expandIo
					- [x] beforeIo
					- [x] mergeIo
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] CONSTRAINT
		- [x] PLAIN
			* [x] TYPE
			* [x] COMPOSER
				+ [x] retry
				+ [x] collapse
				+ [x] expand
				+ [x] before
				+ [x] merge
		- [x] WITH STATE
			* [x] TYPE
				+ [x] type HandleSt'
				+ [x] function liftHandle'
			* [x] COMPOSER
				+ [x] retrySt
				+ [x] expandSt
				+ [x] beforeSt
				+ [x] mergeSt
		- [x] WITH INPUT AND OUTPUT
			* [x] TYPE
				+ [x] type HandleIo'
				+ [x] function pushInput
				+ [x] function popInput
			* [x] COMPOSER
				+ [x] function collapseIo
				+ [x] function expandIo
				+ [x] function beforeIo
				+ [x] function mregeIo
* [x] Control.Moffy.Run
	+ [x] API
		- [x] Type
		- [x] Run
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] SIG
		- [x] REACT
* [x] Control.Moffy.Event.ThreadId
	+ [x] API
	+ [x] extension
	+ [x] imports
	+ [x] body
		- [x] data GetThreadId
		- [x] numbered
		- [x] instance Request
		- [x] function getThreadId
* [x] Control.Moffy.Handle.ThreadId
	+ [x] API
	+ [x] extension
	+ [x] imports
	+ [x] body

## Moffy library (16)

### module hierarchy

```
Control.Moffy.Event
  + Control.Moffy.Event.Lock
	+ Control.Moffy.Event.Lock.Internal
  + Control.Moffy.Event.Random
	+ Control.Moffy.Event.Random.Internal
  + Control.Moffy.Event.Time
  + Control.Moffy.Event.Delete
  + Control.Moffy.Event.Key
      + Control.Moffy.Event.Key.Internal.XK
          + Control.Moffy.Event.Key.Internal
      + Control.Moffy.Event.Key.Internal
  + Control.Moffy.Event.Mouse
Control.Moffy.Handle
  + Control.Moffy.Handle.Lock
  + Control.Moffy.Handle.Random
  + Control.Moffy.Handle.Time
  + Control.Moffy.Handle.XField
      + Control.Moffy.Handle.XField.Key
      + Control.Moffy.Handle.XField.Mouse
```

### refactor modules

* [x] Control.Moffy.Event.Lock
	+ [x] API
		- [x] Type
		- [x] Event
	+ [x] extension
	+ [x] import
* [x] Control.Moffy.Event.Lock.Internal
	+ [x] API
		- [x] Type
			* [x] Type Synonym
			* [x] Event Type
		- [x] Event
	+ [x] extension
	+ [x] import
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
		- [x] Type
		- [x] Handle
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] remove prime
		- [x] LOCK STATE
		- [x] HANDLE
			* [x] handleLock
			* [x] handleNewLockId
			* [x] handleGetLock
			* [x] handleUnlock
* [x] Control.Moffy.Event.Random
	+ [x] API
		- [x] Type
		- [x] Get Random
	+ [x] extension
	+ [x] import
* [x] Control.Moffy.Event.Random.Internal
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] EVENT
			* [x] STORE RANDOM GEN
			* [x] LOAD RANDOM GEN
		- [x] RANDOM EV AND GET RANDOM
* [x] Control.Moffy.Handle.Random
	+ [x] API
		- [x] Type
			* [x] type RandomEv
			* [x] class RandomState
		- [x] Handle
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] RANDOM STATE
		- [x] HANDLE
* [x] Control.Moffy.Event.Time
	+ [x] API
		- [x] structure
		- [x] Type Ev
		- [x] Delta Time
			* [x] data DeltaTime
			* [x] pattern OccDeltaTime
			* [x] function deltaTime
		- [x] Sleep
			* [x] newtype TryWait
			* [x] pattern OccTryWait
			* [x] function sleep
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] DELTA TIME
		- [x] SLEEP
		- [x] TIME EV
* [x] Control.Moffy.Handle.Time
	+ [x] consider whether or not to rename Mode
	+ [x] rename InitMode to InitialMode
	+ [x] API
		- [x] structure
		- [x] Class
			* [x] TimeState
			* [x] TaiTimeM
			* [x] DelayM
		- [x] Mode
		- [x] Handle
	+ [x] extension
	+ [x] import
	+ [x] structure and body
		- class
			* TimeState
				+ Mode
			* TaiTimeM
			* DelayM
		- function
			* handleTimeEvPlus
			* handleInit
			* handleWait
			* handletime
	+ [x] body
		- [x] CLASS
			* [x] TIME STATE
			* [x] TAI TIME MONAD
			* [x] DELAY MONAD
		- [x] HANDLE
			* [x] handleTimeEvPlus
			* [x] handleInit
			* [x] handleWait
			* [x] handleTime
* [x] Control.Moffy.Event.Delete
	+ [x] API
		- [x] Type
		- [x] Event
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Control.Moffy.Event.Key
	+ [x] API
		- [x] Key Ev
		- [x] Key Down Event
		- [x] Key Up Event
		- [x] Key
		- [x] module Control.Moffy.Event.Key.Internal.XK
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] EVENT
			* [x] KEY DOWN
			* [x] KEY UP
			* [x] KEY EV
		- [x] PATTERN
* [x] Control.Moffy.Event.Key.Internal
	+ [x] API
		- [x] Type
		- [x] Template
			* [x] Void Symbol
			* [x] Tty Function Keys
			* [x] Japanese Keyboard
			* [x] Cursor Control & Motion
			* [x] Misc Function
			* [x] Auxiliary functions: F1 to F35
			* [x] Modifiers
			* [x] Keyboard (XKB) Extension Function and Modifier Keys
			* [x] Latin 1 (only Ascii)
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TYPE AND AUXILIARY FUNCTION
		- [x] DEFINITION OF PATTERN XK_FOO
			* [x] VOID SYMBOL
			* [x] TTY FUNCTION KEYS
			* [x] JAPANESE KEYBOARD SUPPORT
			* [x] CURSOR CONTROL AND MOTION
			* [x] MISC FUNCTION
			* [x] AUXILIARY FUNCTION
			* [x] MODIFIERS
			* [x] KEYBOARD (XKB) EXTENSION FUNCTION AND MODIFIER KEYS
			* [x] LATIN 1 (only ASCII)
* [x] Control.Moffy.Event.Key.Internal.XK
	+ [x] API
		- [x] Void Symbol
		- [x] Tty Function Keys
		- [x] Japanese Keyboard
		- [x] Cursor Control & Motion
		- [x] Misc Function
		- [x] Ausiliary Functions: F1 to F35
		- [x] Modifiers
		- [x] Keyboard (Xkb) Extension Function and Modifier Keys
		- [x] Latin 1 (Only Ascii)
			* [x] Space to Slash
			* [x] Digit
			* [x] Colon to At
			* [x] Upper Alphabet
			* [x] Blacketleft to Grave
			* [x] Lower Alphabet
			* [x] Braceleft to Asciitilde
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] VOID SYMBOL
		- [x] NOT VISIBLE
		- [x] ASCII
* [x] Control.Moffy.Event.Mouse
	+ [x] consider whether or not to unlist [MouseBtn]
	+ [x] API
		- [x] structure
		- [x] Type
			* [x] MouseEv
			* [x] MouseBtn
				+ [x] rename and add buttons
				+ [x] others
			* [x] Point
		- [x] Mouse Down
			* [x] MouseDown
			* [x] OccMouseDown
			* [x] mouseDown
			* [x] leftClick
			* [x] middleClick
			* [x] rightClick
		- [x] Mouse Up
			* [x] MouseUp
			* [x] OccMouseUp
			* [x] mouseUp
			* [x] leftUp
			* [x] middleUp
			* [x] rightUp
		- [x] Mouse Move
			* [x] MouseMove
			* [x] OccMouseMove
			* [x] mouseMove
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		+ [x] MOUSE DOWN
			- [x] data MouseDown
			- [x] numbered MouseDown
			- [x] instance Request MouseDown
			- [x] data MouseBtn
			- [x] function mouseDown
			- [x] function clickOn
			- [x] function leftClick, middleClick, rightClick
		+ [x] MOUSE UP
			- [x] data MouseUp
			- [x] numbered MouseUp
			- [x] instance Request MouseUp
			- [x] function mouseUp
			- [x] function releaseOn
			- [x] function leftUp, middleUp, rightUp
		+ [x] MOUSE MOVE
			- [x] data MouseMove
			- [x] numbered MouseMove
			- [x] instance Request MouseMove
			- [x] type Point
			- [x] function mouseMove
		+ [x] MOUSE EV
* [x] Control.Moffy.Handle.XField
	+ [x] API
		- [x] structure
		- [x] Type
		- [x] Handle
			* [x] function handle
			* [x] function handleWith
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] GUI EV
		- [x] HANDLE
			* [x] function handle
			* [x] function handleWith
			* [x] function eventToEv
* [x] Control.Moffy.Handle.XField.Key
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Control.Moffy.Handle.XField.Mouse
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body

Moffy Base (9)
----------

### Control.Monad.Freer.Par (6)

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

#### repair

* [x] repair tag system
	+ [x] make package try-freer-par
	+ [x] copy from package try-freer-par

#### refactoring

* [x] Control.Monad.Freer.Par
	+ [x] API
		- [x] rename Unique
		- [x] consider whether or not to rename qApp and qAppPar
			* qApp -> app
			* qAppPar -> appPar
		- [x] structure
		- [x] Freer
			* [x] Type
				+ [x] data Freer
				+ [x] data Fun
			* [x] Pattern
				+ [x] pattern Pure
				+ [x] pattern (:>>=)
				+ [x] pattern (:=<<)
			* [x] Bind
				+ [x] operator (>>>=)
				+ [x] operator (=<<<)
			* [x] Apply
				+ [x] function app
				+ [x] function appPar
		- [x] Tagged
			* [x] data Tagged
			* [x] function runTagged
			* [x] function tag
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] consider whether or not add fixity of (::>>=), (<|) and (|>)
	+ [x] consider whether or not add fixity of (>>>=) and (=<<<)
	+ [x] body
		- [x] PARALLEL FREER
			* [x] TYPE AND MONAD
				+ [x] data Freer
				+ [x] function freer
				+ [x] instance Functor
				+ [x] instance Applicative
				+ [x] instance Monad
				+ [x] newtype Fun
			* [x] PATTERN
				+ [x] pattern Pure
				+ [x] pattern (:>>=)
				+ [x] pattern (:=<<)
			* [x] BIND
				+ [x] operator (>>>=)
				+ [x] operator (=<<<)
			* [x] APPLICATION
				+ [x] function app
				+ [x] function appPar
				+ [x] function aps
				+ [x] function aps'
				+ [x] function apsPar
		- [x] TAGGED
			* [x] newtype Tagged
			* [x] instance Functor
			* [x] instance Applicative
			* [x] instance Monad
			* [x] function runTagged
			* [x] function tag
* [x] Control.Monad.Freer.Par.Sequence
	+ [x] API
		- [x] structure
		- [x] Sequence and ViewL
		- [x] Combinator
	+ [x] extension
	+ [x] structure
	+ [x] body
		- [x] SEQUENCE AND VIEWL
		- [x] COMBINATOR
* [x] Control.Monad.Freer.Par.Funable
	+ [x] API
		- [x] structure
		- [x] Funable
		- [x] Taggalble
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] class Funable
		- [x] class Taggable
		- [x] data Tag
		- [x] function sameTag
* [x] Control.Monad.Freer.Par.Internal.Id
	+ [x] API
	+ [x] import
	+ [x] body
* [x] Control.Monad.Freer.Par.FTCQueue
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Control.Monad.Freer.Par.TaggableFunction
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] data TaggableFun
		- [x] instance Funable TaggableFun
		- [x] instance Taggable TaggableFun

### One Or More (1)

#### module hierarchy

```
Data.OneOrMore
```

#### refactor

* [x] Data.OneOrMore
	+ [x] API
		- [x] structure
		- [x] Type
		- [x] Property
			* [x] Basic Property
				+ [x] Projectable
				+ [x] Insertable
			* [x] Expandable and Collapsable
				+ [x] Expandable
				+ [x] Collapsable
			* [x] Mergeable
				+ [x] Mergeable
				+ [x] Selectable
		- [x] Function
			* [x] Single Type
				+ [x] pattern Singleton
				+ [x] function unSingleton
			* [x] Multiple Type
				+ [x] function project
				+ [x] operator (>-)
			* [x] Expand and Collapse
				+ [x] expand
				+ [x] collapse
			* [x] Merge
				+ [x] merge
				+ [x] merge'
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] ONE OR MORE TYPE
		- [x] BASIC PROPERTY
			* [x] PROJECTABLE
				+ [x] class Projectable
				+ [x] pattern Singleton
				+ [x] function unSingleton
			* [x] INSERTABLE
		- [x] EXPANDABLE AND COLLAPSABLE
			* [x] EXPANDABLE
				+ [x] class Expandable
				+ [x] class Nihil
			* [x] COLLAPSABLE
				+ [x] COLLAPSABLE 0
				+ [x] COLLAPSABLE
		- [x] MERGEABLE
			* [x] class Mergeable
			* [x] class Selectable
			* [x] function merge'

### Type Set (2)

#### module hierarchy

```
Data.Type.Set
  +- Data.Type.Set.Internal
```

#### refactor

* [x] Data.Type.Set
	+ [x] API
		- [x] structure
		- [x] Set
		- [x] Numbered
		- [x] Function
		- [x] Operator
	+ [x] import
* [x] Data.Type.Set.Internal
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TYPE SET
			* [x] DATA DEFINITION
			* [x] COMBINATOR
				+ [x] Singleton
				+ [x] Insert
				+ [x] Merge
				+ [x] Map
		- [x] NUMBERED
		- [x] BOOL

Trials (20)
------

### tribial (10)

#### TrySharing

##### repair

* [x] use showButton
* [x] remove old functions

##### addition

* [x] add nest first trial

##### refactoring

* [x] Trial.TrySharing
	+ [x] rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] NO SHARING
		- [x] SHARING
			* [x] SIMPLE
			* [x] NEST FIRST'
				+ [x] function runSharingShowButton4
				+ [x] function runSharingShowButton8
			* [x] TWO TIME CLICK
		- [x] TOOLS
			* [x] TYPE OR'
			* [x] RUN MOUSE EV
			* [x] SHOW BUTTON
				+ [x] function showButton
				+ [x] function show'
				+ [x] message

#### CheckSharing.EvInt

##### refactoring

* CheckSharing.EvInt
	+ [x] remove it

#### TrySharing.ThreadId

* [x] TrySharing.ThreadId
	+ [x] view
	+ [x] rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TRIAL
			* [x] action runFirstGetThreadId
			* [x] action runSharingFirstGetThreadId
			* [x] action runSharingFirstGetThreadId'
		- [x] PARTS
			* [x] type Or'
			* [x] runMouseThreadId
			* [x] react heavyGetThreadId
			* [x] react heavyMouseDown
			* [x] function heavyId

#### Count

##### refactoring

* [x] Trial.Count
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TRIAL
			* [x] action tryLeftCount
			* [x] action tryLeftCountSig
			* [x] action tryLeftRandomSig
		- [x] REACT AND SIG
			* [x] function leftCount
			* [x] function leftCountSig
			* [x] function leftRandomSig
		- [x] RUN
			* [x] runMouseReact
			* [x] runMouse

#### Try Key

* [x] Trial.TryKey
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] action tryKey
		- [x] sig keySig
		- [x] react asciiKey
		- [x] react asciiKeyUp

#### Try ThreadId

* [x] Trial.ThreadId
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] trySingleThreadId
		- [x] tryDoubleThreadId
		- [x] runGetThreadId
		- [x] tryLeftRightThreadId
		- [x] tryLeftRightThreadId'
		- [x] runMouseGetThreadId
		- [x] clickThenGetThreadId

#### Try Lock

* [x] Trial.TryLock
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] LOCK ST
			* [x] data LockSt
			* [x] instance LockState LockSt
		- [x] TRIAL
			* [x] SINGLE
				+ [x] action trySingleLeftCount
				+ [x] function leftCount
			* [x] NO LOCK
			* [x] LOCK
				+ [x] action tryLockLeftCount2
				+ [x] function lockLeftCount
		- [x] RUN
			* [x] function runClick
			* [x] function runClickLockSt

#### Try Random

##### refactoring

* [x] Trial.TryRandom
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] import
	+ [x] body
		- [x] list diceTrial
		- [x] function evalRandom
		- [x] function getRandomRs

#### Count With Lock

* [x] Trial.CountWithLock
	+ [x] view
	+ [x] consider whether or not to remove

### Boxes (6)

#### module hierarchy

```
Main
  +- Trial.Boxes
  |    +- Trial.Boxes.Box
  |    +- Trial.Boxes.BoxEv
  +- Trial.Boxes.Run
       +- Trial.Boxes.Handle
       |    +- Trial.Boxes.BoxEv
       +- Trial.Boxes.View
            +- Trial.Boxes.Box
```

#### refactoring

* [x] refactor module name and hierarchy
* [x] check module hierarchy
* [x] refactor each modules

##### module Trial.StepByStepBox

* [x] view
* [x] separate to Trial.Boxes and other
	+ [x] create empty module Trial.Boxes
	+ [x] others
* [x] separate to Trial.Boxes and other
	+ [x] create empty module Trial.Boxes.View
	+ [x] others
* [x] rename to Trial.Boxes.Run
	+ [x] rename
	+ [x] API
* [x] repair app/Main.hs

##### each modules

* [x] Main
	+ [x] import
	+ [x] body
		- [x] rename trySigGBoxes' to runBoxes
* [x] Trial.Boxes
	+ [x] view
	+ [x] consider
		- [x] whether or not to move elapsed to Control.Moffy.Event.Time
		- [x] whether or not to move mousePos to Control.Moffy.Event.Mouse
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] BOXES
			* [x] sig boxes
			* [x] sig box
		- [x] DEFINE RECT
			* [x] sig defineRect
			* [x] react firstPoint
			* [x] function completeRect
			* [x] value neverOccur
		- [x] CHOOSE BOX COLOR
			* [x] function chooseBoxColor
			* [x] function wiggleRect
			* [x] sig cycleColor
		- [x] DR CLICK ON
			* [x] function drClickOn
			* [x] react doubler
		- [x] BEFORE
	+ [x] import
* [x] Trial.Boxes.Box
	+ [x] API
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.BoxEv
	+ [x] API
		- [x] rename SigG, ISigG and ReactG to SigB, ISigB and ReactB
		- [x] others
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.Run
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.Handle
	+ [x] API
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.View
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] drawBoxes
		- [x] drawBox
		- [x] drawREct
		- [x] withFlush
* [x] consider unify module Trial.Boxes.Run and Trial.Boxes.Handle
* [x] Trial.Boxes.Run
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body

### Try Check Dup

* [x] Trial.TryCheckDup
	+ [x] consider whether or not to remove
		- [x] remove doubler from export list of Trial.Boxes

### Followbox (8)

#### module hierarchy

```
Main
  +- Trial.Followbox
  |    +- Trial.Followbox.Event
  |    |    +- Trial.Followbox.TypeSynonym
  |    +- Trial.Followbox.Clickable
  |    |    +- Trial.Followbox.Event
  |    |    +- Trial.Followbox.ViewType
  |    |    +- Trial.Followbox.TypeSynonym
  |    +- Trial.Followbox.ViewType
  |    |    +- Trial.Followbox.TypeSynonym
  |    +- Trial.Followbox.TypeSynonym
  +- Trial.Followbox.Run
       +- Trial.Followbox.Handle
       |    +- Trial.Followbox.Event
       |    +- Trial.Followbox.TypeSynonym
       +- Trial.Followbox.View
       |    +- Trial.Followbox.ViewType
       +- Trial.Followbox.TypeSynonym
```

#### refactoring

* [x] refactor module dependencies
	+ [x] make module Trial.Followbox not to depend Trial.Followbox.View
		- [x] separate Trial.Followbox.View to itself and Trial.Followbox.ViewType
		- [x] Trial.Followbox import ....ViewType instead of ...View
	+ [x] rename Trial.Followbox.HandleNew to ...Handle
* [x] Main
	+ [x] import
	+ [x] body
* [x] Trial.Followbox
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] PARAMETER LIST
			* [x] view
			* [x] NUMBER OF USER TO DISPLAY
			* [x] MAX NUMBER OF GITHUB USER
			* [x] BACKGROUND
				+ [x] titlePos
				+ [x] nextPos
				+ [x] refreshPos
				+ [x] resetTimePos
			* [x] FONT
				+ [x] defaultFont
				+ [x] middleSize, largeSize
			* [x] AVATAR, NAME AND CROSS
				+ [x] avatarSizeX, avatarSizeY
				+ [x] avatarPos, namePos
				+ [x] crossSize
				+ [x] crossPos
				+ [x] crossMargin
		- [x] SIG AND REACT
			* [x] FOLLOWBOX
				+ [x] followbox
				+ [x] fieldWithResetTime
				+ [x] field
				+ [x] resetTime
			* [x] USERS
				+ [x] users
				+ [x] user1
				+ [x] cross
			* [x] GET USER
				+ [x] getUser
				+ [x] getAvatar
			* [x] GET OBJECT
				+ [x] getObj1
				+ [x] getObj1FromWeb
				+ [x] getObjs
		- [x] HELPER FUNCTION
			* [x] twhite
			* [x] lwhite
			* [x] posixSeconds
* [x] Trial.Followbox.Clickable
	+ [x] API
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] consider to change view and click to field
		- [x] CLICKABLE
			* [x] data Clickable
			* [x] clickable
		- [x] WITH TEXT EXTENTS
			* [x] data WithTextExtents
			* [x] clickableText
			* [x] withTextExtents
			* [x] nextToText
			* [x] translate
* [x] Trial.Followbox
	+ [x] body
		- [x] SIG AND REACT: FOLLOWBOX: function field
		- [x] SIG AND REACT: USERS: function user1
* [x] Trial.Followbox.Event
	+ [x] API
		- [x] structure
		- [x] Followbox Event
			* [x] type SigF
			* [x] type ReactF
			* [x] type FollowboxEv
		- [x] Store and Load Jsons
			* [x] data StoreJsons
			* [x] pattern OccStoreJsons
			* [x] data LoadJsons
			* [x] pattern OccLoadJsons
			* [x] clearJsons
			* [x] storeJsons
			* [x] loadJsons
		- [x] Request Data
			* [x] Http Get
				+ [x] data HttpGet
				+ [x] pattern OccHttpGet
				+ [x] httpGet
			* [x] Calc Text Extents
				+ [x] data CalcTextExtents
				+ [x] pattern OccCalcTextExtents
				+ [x] calcTextExtents
			* [x] Get Time Zone
				+ [x] data GetTimeZone
				+ [x] pattern OccGetTimeZone
				+ [x] getTimeZone
		- [x] Browse
			+ [x] data Browse
			+ [x] pattern OccBrowse
			+ [x] browse
		- [x] Sleep
			+ [x] data BeginSleep
			+ [x] pattern OccBeginSleep
			+ [x] data EndSleep
			+ [x] pattern OccEndSleep
			+ [x] beginSleep
			+ [x] checkBeginSleep
			+ [x] endSleep
		- [x] Raise Error
			+ [x] data RaiseError
			+ [x] pattern OccRaiseError
			+ [x] data Error
			+ [x] data ErrorResult
			+ [x] raiseError
			+ [x] checkTerminate
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] STORE AND LOAD JSON OBJECT LIST
			* [x] define and instance Request of StoreJsons
			* [x] function clearJsons
			* [x] function storeJsons
			* [x] define and instance Request of LoadJsons
			* [x] functin loadJsons
		- [x] REQUEST DATA
			* [x] HTTP GET
				+ [x] define and instance Request of HttpGet
				+ [x] function httpGet
			* [x] CALC TEXT EXTENTS
				+ [x] define and instance Request of CalcTextExtents
				+ [x] function calcTextExtents
			* [x] TIME ZONE
				+ [x] define and instance Request of GetTimeZone
				+ [x] function getTimeZone
		- [x] BROWSE
			* [x] define and instance Request of Browse
			* [x] function browse
		- [x] SLEEP
			* [x] define and instance Request of BeginSleep
			* [x] function beginSleep
			* [x] function checkBeginSleep
			* [x] define and instance Request of EndSleep
			* [x] function endSleep
		- [x] RAISE ERROR
			* [x] data Error
			* [x] data ErrorResult
			* [x] define and instance Request of RaiseError
			* [x] function raiseError
			* [x] function catchError
			* [x] function checkTerminate
		- [x] FOLLOWBOX EVENT TYPE
			* [x] type SigF
			* [x] type ReactF
			* [x] type FollowboxEv
* [x] Trial.Followbox.TypeSynonym
	+ [x] API
		- [x] structure
		- [x] rename from FOO to Foo
		- [x] Field
			* [x] WindowTitle
			* [x] Position
			* [x] LineWidth
			* [x] FontName
			* [x] FontSize
			* [x] Avatar
		- [x] GitHub
			* [x] GithubNameToken
			* [x] GithubUserName
			* [x] GithubToken
		- [x] Others
			* [x] Uri
			* [x] Browser
			* [x] ErrorMessage
	+ [x] import
	+ [x] structure
	+ [x] body
* [x] Trial.Followbox.Run
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] DEFAULt BROWSER
		- [x] RUN FOLLOWBOX
			* [x] runFollowbox
			* [x] evalFollowbox
			* [x] run
		- [x] GET FOLLOWBOX INFO
			* [x] data FollowboxInfo
			* [x] getFollowboxInfo
			* [x] optionToInfo
		- [x] GET OPT
			* [x] data FollowboxOption
			* [x] chkDupOpt
			* [x] followboxOptions
* [x] Trial.Followbox.Handle
	+ [x] API
		- [x] structure
		- [x] Handle
			* [x] type HandleF
			* [x] handleFollowbox
		- [x] State
			* [x] data FollowboxState
			* [x] initialFollowboxState
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] STATE
			* [x] FOLLOWBOX STATE
				+ [x] data FollowboxState
				+ [x] initialFollowboxState
				+ [x] type HandleF
				+ [x] type HandleF'
			* [x] PUT AND GET EACH STATE
				+ [x] instance LockState FollowboxState
				+ [x] instance RandomState FollowboxState
		- [x] HANDLE
			* [x] FOLLOWBOX
			* [x] MOUSE
			* [x] STORE AND LOAD JSONS
			* [x] REQUEST DATA
				- [x] handleHttpGet
				- [x] handleCalcTextExtents
				- [x] handleGetTimeZone
			* [x] BROWSE
			* [x] BEGIN AND END SLEEP
			* [x] RAISE ERROR
		- [x] HELPER FUNCTION
* [x] Trial.Followbox.View
	+ [x] API
		- [x] View
			* [x] type View
			* [x] data View1
			* [x] function view
		- [x] Color
			* [x] data Color
			* [x] value white
			* [x] value blue
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] VIEW
			* [x] view
			* [x] view1
				+ [x] match Text
				+ [x] match Line
				+ [x] match Image
			* [x] colorToPixel
		- [x] DRAW PIXEL IMAGE
			* [x] drawImagePixel
			* [x] swap02s
* [x] Trial.Followbox.ViewType

ref
---

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```

```
f :: Integer -> Integer -> Rational
f (fromInteger . (2 ^) -> m) n = 1 - product [m - fromInteger n + 1 .. m] / m ^ n

> fromRational . recip $ f 64 10000 :: Double
3.6897177865255615e11
```

task
----
