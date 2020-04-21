memo
====

モジュール構造
--------------

```
Main
  +- Trials.Followbox
  |    +- Trials.Followbox.Event
  |    |    +- Trials.Followbox.Wrapper.Aeson
  |    |    +- Trials.Followbox.Wrapper.XGlyphInfo
  |    +- Trials.Followbox.View
  |    +- Trials.Followbox.Wrapper.Aeson
  +- Trials.Followbox.Run
       +- Trials.Followbox.Handle
       |    +- Trials.Followbox.Event
       |    +- Trials.Followbox.Wrapper.XGlyphInfo
       +- Trials.Followbox.Event
       +- Trials.Followbox.View
```

リファクタリング
----------------

### API

* [x] Trials.Followbox.Event
* [x] Trials.Followbox.Handle
* [x] Trials.Followbox.View
* [x] Trials.Followbox
* [x] Trials.Followbox.Trials

### remove

* [x] Trials.Followbox.TestMonad
* [x] Trials.Followbox.Image
* [x] Trials.Followbox.BasicAuth

### move to sub directory

* [x] Trials.Followbox.Wrapper
	+ [x] Trials.Followbox.Aeson
	+ [x] Trials.Followbox.XGlyphInfo

### Body

* [ ] Trials.Followbox
* [ ] Trials.Followbox.Event
* [ ] Trials.Followbox.Run
* [ ] Trials.Followbox.Handle
* [ ] Trials.Followbox.View
* [ ] Trials.Followbox.Wrappwer.Aeson
* [ ] Trials.Followbox.Wrappwer.XGlyphInfo
