GdkWindowAttr memo
==================

* [x] make `Graphics.Gdk.Windows.GdkWindowAttr.Internal`
* [x] move `GdkWindowTypeHint` to `Graphics.Gdk.Window.GdkWindowAttr.Internal`
* [x] `gdkWindowNew`: should return maybe value
* [x] define `gdkToplevelNew`
* [x] remove window type from minimal
* [x] remove window class from minimal
* [x] refactor export list
	+ [x] structure
	+ [x] GDK WINDOW ATTRIBUTE
		- [x] `data GdkWindowAttr`
			* [x] `gdkWindowAttrTitle`
			* [x] `gdkWindowAttrEventMask`
			* [x] `gdkWindowAttrX`, `gdkWindowAttrY`
			* [x] `gdkWindowAttrWidth`, `gdkWindowAttrHeight`
			* [x] `gdkWindowAttrWclass`
			* [x] `gdkWindowAttrVisual`
				+ [x] draw with translucent green
				+ [x] compare system visual and RGBA visual
			* [x] `gdkWindowAttrWindowType`
			* [x] `gdkWindowAttrCursor`
			* [x] `gdkWindowAttrOverrideRedirect`
			* [x] `gdkWindowAttrTypeHint`
		- [x] `minimalGdkWindowAttr`
	+ [x] GDK WINDOW WINDOW CLASS
	+ [x] GDK WINDOW TYPE
	+ [x] GDK WINDOW TYPE HINT

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
