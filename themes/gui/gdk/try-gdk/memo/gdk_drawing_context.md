GdkDrawingContext memo
======================

* [x] make export list
* [x] refactor export list
* [x] make `Graphics.Gdk.GdkDrawingContext.Internal`
* [x] refactor export list
	+ [x] structure
	+ [x] GDK DRAWING CONTEXT
		- `GdkDrawingContext`
	+ [x] IS VALID
		- `gdkDrawingContextIsValid`
	+ [x] WINDOW AND CLIP
		- [x] `gdkDrawingContextGetWindow`
		- [x] `gdkDrawingContextGetClip`
	+ [x] CAIRO CONTEXT
		- `gdkDarwingContextGetCairoContext`

old
---

* [x] make export list
* [x] `gdkDrawingContextGetWindow`
* [x] `gdkDrawingContextGetClip`
	+ [x] repair `gdkWindowWithDrawFrame`
	+ [x] repair `gdkDrawingContextGetClip`
* [x] `gdkDrawingContextGetCairoContext`
	+ [x] add one more variable to `CairoT`
* [x] `gdkDrawingContextIsValid`
