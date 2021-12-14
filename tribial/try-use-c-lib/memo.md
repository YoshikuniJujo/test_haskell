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
	+ [ ] `Game`
		- [x] import list
		- [x] structure
		- [ ] source code
			* [ ] PARAMETERS
			* [x] GAME STATE
			* [ ] HERO
			* [ ] ENEMY
			* [ ] INPUT
* [ ] refactoring Haskell app sources
	+ [ ] `try-old-event.hs`
	+ [ ] `try-event.hs`
	+ [ ] `try-get-and-push-cchar.hs`
	+ [ ] `try-jump.hs`
	+ [ ] `try-game.hs`
