memo
====

モジュール構造
--------------

```
Main
  +- Trials.Followbox
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

* [ ] Trials.Followbox.TypeSynonym
* [ ] Trials.Followbox.ThreadId
* [ ] Trials.Followbox.Random
* [ ] Trials.Followbox.Event
* [ ] Trials.Followbox
	+ [ ] imports
	+ [ ] body
* [ ] Trials.Followbox.View
* [ ] Trials.Followbox.Handle
* [ ] Trials.Followbox.Run
