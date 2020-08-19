memo
====

structure
---------

* Moffy (9)
	+ Control.Moffy
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
	+ [ ] Control.Moffy.Handle
	+ [x] Moffy
	+ [x] Moffy library
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
* [ ] consider whether or not to put `deriving Show' to Occurred Foo


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
		- [x] Class
		- [x] Constraint
			* [x] Firstable
			* [x] Adjustable
		- [x] Function
			* [x] first
			* [x] adjust
			* [x] par
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FIRST
		- [x] ADJUST
		- [x] PAR
		- [x] UPDATE
			* [x] class
			* [x] instance a a
			* [x] instance a b
* [x] Control.Moffy.Internal.React.Type
	+ [x] API
		- [x] React
			* [x] Type
				+ [x] type React
				+ [x] data Rct
				+ [x] class Request
				+ [x] type EvReqs
				+ [x] type EvOccs
			* [x] Never and Await
				+ [x] never
				+ [x] await
				+ [x] await'
		- [x] Handle
			* [x] type Handle
			* [x] type HandleSt
			* [x] function liftHandle
		- [x] St
			* [x] type St
			* [x] function liftSt
		- [x] ThreadId
			* [x] data ThreadId
			* [x] function rootThreadId
			* [x] react forkThreadId
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] REACT
			* [x] TYPE
			* [x] NEVER AND AWAIT
		- [x] HANDLE
		- [x] ST
		- [x] THREAD ID
* [x] Control.Moffy.Handle
	+ [x] API
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
					- [x] liftHandle
					- [x] liftHandle'
					- [x] St
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
		- [x] WITH STATE
			* [x] TYPE
			* [x] COMPOSER
				+ [x] retrySt
				+ [x] expandSt
				+ [x] beforeSt
				+ [x] mergeSt
		- [x] WITH INPUT AND OUTPUT
			* [x] TYPE
			* [x] COMPOSER
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
* [ ] Control.Monad.Freer.Par.Funable
	+ [ ] API
	+ [ ] extension
	+ [ ] import
	+ [ ] structure
	+ [ ] body
* [ ] Control.Monad.Freer.Par.Internal.Id
	+ [ ] API
	+ [ ] extension
	+ [ ] import
	+ [ ] structure
	+ [ ] body
* [ ] Control.Monad.Freer.Par.FTCQueue
* [ ] Control.Monad.Freer.Par.TaggableFunction

### One Or More (1)

#### module hierarchy

```
Data.OneOrMore
```

#### refactor

### Type Set (2)

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

```
f :: Integer -> Integer -> Rational
f (fromInteger . (2 ^) -> m) n = 1 - product [m - fromInteger n + 1 .. m] / m ^ n

> fromRational . recip $ f 64 10000 :: Double
3.6897177865255615e11
```

task
----
