memo
====

モジュール構造
--------------

```
Main
  +- Trials.Boxes
  |    +- Trials.Boxes.Events
  |    |    +- Trials.Boxes.Mouse
  |    +- Trials.Boxes.View
  +- Trials.Boxes.Run
       +- Trials.Boxes.Handle
       |    +- Trials.Boxes.Events
       +- Trials.Boxes.View
```

リファクタリング
----------------

* [ ] module hierarchy
	+ [ ] Trials.Boxes.Mouse
	+ [ ] Trials.Boxes.Mouse
		- -> MonadicFrp.Events.Mouse
		- -> MonadicFrp.XHandle.Mouse

### API

* [ ] Trials.Boxes.Events
	+ [x] 1st
	+ [ ] 2nd
		- [ ] Occurred ?
		- [ ] others
* [ ] Trials.Boxes.Mouse
* [ ] Trials.Boxes
* [ ] Trials.Boxes.Handle
* [ ] Trials.Boxes.TryReact
* [ ] Trials.Boxes.TrySig

### Body

* [ ] Trials.Boxes.Events
* [ ] Trials.Boxes
* [ ] Trials.Boxes.Handle
* [ ] Trials.Boxes.TryReact
* [ ] Trials.Boxes.TrySig
* [ ] Main
