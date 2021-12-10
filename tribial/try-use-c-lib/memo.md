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
* [ ] refactoring `include`
	+ [ ] `human.h`
	+ [ ] `mem_alloc.h`
	+ [ ] `mem_alloc_local.h`
* [ ] refactoring `csrc`
	+ [ ] `human.c`
	+ [ ] `mem_alloc.c`
	+ [ ] `mem_alloc_draw.c`
* [ ] refactoring Haskell library sources
	+ [ ] `Human`
	+ [ ] `Human.Exception`
	+ [ ] `Human.EventOld`
	+ [ ] `Human.Event`
	+ [ ] `MainLoop`
	+ [ ] `Game`
* [ ] refactoring Haskell app sources
	+ [ ] `try-old-event.hs`
	+ [ ] `try-event.hs`
	+ [ ] `try-get-and-push-cchar.hs`
	+ [ ] `try-jump.hs`
	+ [ ] `try-game.hs`
