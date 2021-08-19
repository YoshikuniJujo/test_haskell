GdkWindowAttr memo
==================

* [x] make `Graphics.Gdk.Windows.GdkWindowAttr.Internal`
* [ ] refactor export list
	+ [x] structure
	+ [ ] GDK WINDOW ATTRIBUTE
	+ [ ] GDK WINDOW WINDOW CLASS
	+ [ ] GDK WINDOW TYPE

todo
----

* [x] `newGdkWindowAttr` use `GdkWindowAttributesTypes` instead of `Word32`
	+ [x] define `GdkWindowAttributesTypes`
	+ [x] define function of type `[GdkWindowAttributesType] -> GdkWindowAttributesTypes`
* [x] rename `mkGdkWindowAttr` to `minimalGdkWindowAttr`
* [x] remove `newGdkWindowAttr`
* [x] make `withGdkWindowAttr`
* [x] rename `fpoke1` to `setAttributes`
* [x] review it
	+ [x] do
	+ [x] more
* [x] check title is copy or not
	+ [x] make sample code
* [x] check cursor is copy or not
