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
  |    +- Trials.Followbox.View
  +- Trials.Followbox.Run
       +- Trials.Followbox.Handle
       |    +- Trials.Followbox.Event
       |    +- Trials.Followbox.Random
       +- Trials.Followbox.Event
       +- Trials.Followbox.View
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
* [ ] Trials.Followbox.View
* [ ] Trials.Followbox
* [ ] Trials.Followbox.Trials

### remove

* [x] Trials.Followbox.Wrapper.Aeson
* [x] Trials.Followbox.Wrapper.XGlyphInfo

### move to sub directory

### Body

* [ ] Trials.Followbox
	+ [ ] imports
	+ [ ] body
* [ ] Trials.Followbox.ThreadId
* [ ] Trials.Followbox.Random
* [ ] Trials.Followbox.Event
* [ ] Trials.Followbox.Run
* [ ] Trials.Followbox.Handle
* [ ] Trials.Followbox.View
