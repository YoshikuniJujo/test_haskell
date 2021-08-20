GdkWindowAttr memo
==================

* [x] make `Graphics.Gdk.Windows.GdkWindowAttr.Internal`
* [x] move `GdkWindowTypeHint` to `Graphics.Gdk.Window.GdkWindowAttr.Internal`
* [ ] refactor export list
	+ [x] structure
	+ [ ] GDK WINDOW ATTRIBUTE
		- [ ] `data GdkWindowAttr`
			* [x] `gdkWindowAttrTitle`
			* [x] `gdkWindowAttrEventMask`
			* [ ] `gdkWindowAttrX`, `gdkWindowAttrY`
			* [x] `gdkWindowAttrWidth`, `gdkWindowAttrHeight`
			* [x] `gdkWindowAttrWclass`
			* [x] `gdkWindowAttrVisual`
				+ [x] draw with translucent green
				+ [x] compare system visual and RGBA visual
			* [ ] `gdkWindowAttrWindowType`
				+ [ ] consider to remove types other than toplevel
				+ [ ] others
			* [x] `gdkWindowAttrCursor`
			* [x] `gdkWindowAttrOverrideRedirect`
			* [x] `gdkWindowAttrTypeHint`
		- [ ] `minimalGdkWindowAttr`
	+ [ ] GDK WINDOW WINDOW CLASS
	+ [ ] GDK WINDOW TYPE
	+ [ ] GDK WINDOW TYPE HINT

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
