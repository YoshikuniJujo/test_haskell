gdkEventGet and Segmentation fault memo
=======================================

plan 1
------

* [x] define `GdkEventSealed s`
* [x] make module `Data.Sealed`
* [x] define `Sealed s a`
* [x] rename `GdkEventFoo` to `GdkEventFooRaw`
	+ [x] `GdkEventAny`
	+ [x] `GdkEventKey`
	+ [x] `GdkEventMotion`
* [ ] define `GdkEventFoo`
* [ ] define converter whose type is `Sealed s GdkEventFooRaw -> GdkEventFoo`
* [ ] define converter from `GdkEventRaw s` to `GdkEvent`
* [ ] define `gdkWithEvent`
* [ ] use `gdkWithEvent`
* [ ] remove `gdkEventGet`
* [ ] remove `GdkEvent`

todo
----

* [x] check `try-motion`
* [x] make simple trial
* [x] make no problem trial

plan 2 - bad
------------

* [x] copy in `gdkEventGet`
