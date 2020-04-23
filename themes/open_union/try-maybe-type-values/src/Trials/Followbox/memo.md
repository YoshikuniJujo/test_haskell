memo
====

モジュール構造
--------------

```
Main
  +- Trials.Followbox
  |    +- Trials.Followbox.Event
  |    |    +- Trials.Followbox.Random
  |    |    |    +- Trials.TryThreadId
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

* [ ] module hierarchy
	+ [ ] move Trials.TryThreadId -> Trials.Followbox.ThreadId

### API

* [ ] Trials.Followbox.Event
* [ ] Trials.Followbox.Handle
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
* [ ] Trials.Followbox.Event
* [ ] Trials.Followbox.Run
* [ ] Trials.Followbox.Handle
* [ ] Trials.Followbox.View
* [ ] Trials.Followbox.Wrappwer.Aeson
* [ ] Trials.Followbox.Wrappwer.XGlyphInfo
