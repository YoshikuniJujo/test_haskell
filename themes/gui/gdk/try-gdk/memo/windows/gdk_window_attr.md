GdkWindowAttr memo
==================

todo
----

* [x] `newGdkWindowAttr` use `GdkWindowAttributesTypes` instead of `Word32`
	+ [x] define `GdkWindowAttributesTypes`
	+ [x] define function of type `[GdkWindowAttributesType] -> GdkWindowAttributesTypes`
* [x] rename `mkGdkWindowAttr` to `minimalGdkWindowAttr`
* [x] remove `newGdkWindowAttr`
* [x] make `withGdkWindowAttr`
* [x] rename `fpoke1` to `setAttributes`
* [ ] review it
	+ [x] do
	+ [ ] more
* [ ] check title is copy or not
* [ ] check cursor is copy or not
