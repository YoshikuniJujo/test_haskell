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
	+ [ ] more
* [x] make `Data.Sealed.Internal`
* [ ] define `GdkEventFoo`
	+ [ ] `GdkEventAny`
	+ [ ] `GdkEventKey`
	+ [ ] `GdkEventMotion`
	+ [ ] more
* [ ] define converter whose type is `Sealed s GdkEventFooRaw -> GdkEventFoo`
	+ [ ] `GdkEventAny`
	+ [x] `GdkEventKey`
	+ [ ] `GdkEventMotion`
	+ [ ] more
* [ ] define converter from `GdkEventSealed s` to `Sealed s GdkEventFooRaw`
	+ [ ] `GdkEventAny`
	+ [x] `GdkEventKey`
	+ [ ] `GdkEventMotion`
	+ [ ] more
* [x] define `gdkWithEvent`
* [x] use `gdkWithEvent`
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
