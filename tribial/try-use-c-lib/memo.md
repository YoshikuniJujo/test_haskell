memo
====

* [x] `hm_field_destroy`
* [x] Field: use `ForeignPtr` instead of `Ptr`
* [x] rename `HmDrawHumanResult` to `HmPutHumanResult`
* [x] ST monad
* [x] primitive
* [x] make `hm_event_destroy`
* [x] make `getEvent`
	+ set finalizer
* [x] define `pattern EventTick`
* [x] define C key event
	+ use queue
* [x] test GC
* [x] separate old Event to Human.OldEvent
* [x] make new Event structure
	+ use `Foo s` pattern
* [x] define `pattern EventTick`
* [x] add functions to `include/human.h`
* [x] refactoring `include`
	+ [x] `human.h`
	+ [x] `mem_alloc.h`
	+ [x] `mem_alloc_local.h`
* [x] refactoring `csrc`
	+ [x] `human.c`
		- [x] header
		- [x] SHAPE OF HUMAN
		- [x] FIELD 0
		- [x] MULTIPLE FIELDS
		- [x] IMAGE
		- [x] VARIOUS HUMANS
		- [x] EVENT
		- [x] BACKGROUND
		- [x] MESSAGE
	+ [x] `mem_alloc.c`
	+ [x] `mem_alloc_draw.c`
* [ ] refactoring Haskell library sources
	+ [x] `Human`
		- [x] import list
		- [x] structure
		- [x] source code
			* [x] FIELD
			* [x] SHAPE OF HUMAN
			* [x] FIELD 0
			* [x] MULTIPLE FIELDS
			* [x] IMAGE
			* [x] VARIOUS HUMANS
			* [x] BACKGROUND
			* [x] MESSAGE
	+ [x] `Human.Exception`
	+ [x] `Human.EventOld`
	+ [x] `Human.Event`
		- [x] import list
		- [x] structure
		- [x] source code
			* [x] EVENT
			* [x] EVENT TYPE
			* [x] EVENT TICK
			* [x] EVENT CHAR
			* [x] READ HANDLE
	+ [x] `MainLoop`
	+ [x] `Game`
		- [x] import list
		- [x] structure
		- [x] source code
			* [x] PARAMETERS
			* [x] GAME STATE
			* [x] HERO
				+ [x] data
				+ [x] heroY
				+ [x] putHero
				+ [x] hero Left, Right and Jump
				+ [x] heroStep
			* [x] ENEMY
				+ [x] data
				+ [x] enemyOut
				+ [x] putEnemy
				+ [x] enemyForward
			* [x] ENEMIES
				+ [x] enemiesStep
				+ [x] enemyEnergyAdd
				+ [x] calcEnemyEnergy
				+ [x] enemiesMove
			* [x] INPUT
				+ [x] data
				+ [x] gameInput
				+ [x] gameTick
				+ [x] checkBeat and checkOverlap
* [x] refactoring Haskell app sources
	+ [x] `try-old-event.hs`
	+ [x] `try-event.hs`
	+ [x] `try-get-and-push-cchar.hs`
	+ [x] `try-jump.hs`
	+ [x] `try-game.hs`
