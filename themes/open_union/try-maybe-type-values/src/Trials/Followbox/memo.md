memo
====

モジュール構造
--------------

```
Main
  +- Trials.Followbox
  |    +- Trials.Followbox.Clickable
  |    +- Trials.Followbox.Event
  |    |    +- Trials.Followbox.Random
  |    |    |    +- Trials.Followbox.ThreadId
  |    |    +- Trials.Followbox.TypeSynonym
  |    +- Trials.Followbox.View
  |         +- Trials.Followbox.TypeSynonym
  +- Trials.Followbox.Run
       +- Trials.Followbox.Handle
       |    +- Trials.Followbox.Event
       |    +- Trials.Followbox.Random
       |    +- Trials.Followbox.TypeSynonym
       +- Trials.Followbox.Event
       +- Trials.Followbox.View
       +- Trials.Followbox.TypeSynonym
```

リファクタリング
----------------

* [x] module hierarchy
	+ [x] move Trials.TryThreadId -> Trials.Followbox.ThreadId

### API

* [x] Trials.Followbox.ThreadId
* [x] Trials.Followbox.Random
* [x] Trials.Followbox.Event
* [x] Trials.Followbox.Handle
* [x] Trials.Followbox.TypeSynonym
* [x] Trials.Followbox.View
* [x] Trials.Followbox
* [x] Trials.Followbox.Run

### remove

### move and rename

### Body

* [x] Trials.Followbox.TypeSynonym
* [x] Trials.Followbox.ThreadId
* [x] Trials.Followbox.Random
	+ [x] imports
	+ [x] body
* [x] Trials.Followbox.Event
	+ [x] imports
	* [x] structure
	+ [x] body
* [x] Trials.Followbox.Clickable
* [x] Trials.Followbox
	+ [x] imports
		- [x] 1st
		- [x] snd
	+ [x] structure
	+ [x] body
		- [x] until user1
		- [x] rest
		- [x] add no header errors
		- [x] replace magic number with symbolic constant
		- [x] 2nd
		- [x] 3rd
* [x] Trials.Followbox.View
* [ ] Trials.Followbox.Handle
	+ [x] imports
	+ [x] consider to use NotEmpty
	+ [x] structure
	+ [x] body until \`HTTP GET'
	+ [x] body from \`CALC TEXT EXTENTS' until \`BROWSE'
	+ [x] body \`BEGIN AND END SLEEP'
	+ [ ] body from \`RAISE ERROR'
* [ ] Trials.Followbox.Run

### Correct

* [ ] try to use ThreadId to load objects
