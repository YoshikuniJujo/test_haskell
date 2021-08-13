Cursors memo
============

* [x] make `Graphics.Gdk.Cursors.Internal`
* [x] refactor export list
	+ [x] structure
	+ [x] GDK CURSOR
	+ [x] DISPLAY
	+ [x] CURSOR NEW
		- [x] From Surface
			* [x] try `cairoFormatArgb32`
			* [x] try `cairoFormatRgb24`
			* [x] try `cairoFormatA8`
			* [x] try `cairoFormatA1`
			* [x] try `cairoFormatRgb16565`
			* [x] try `cairoFormatRgb30`
		- [x] From Name
		- [x] From Cursor Type
			* [x] gdkCursorNewForDisplay
				+ [x] check no cursor type
				+ [x] repair it
			* [x] gdkCursorGetCursorType
				+ [x] repair it
				+ [x] others

Function
--------

### now

* [x] `gdk_cursor_get_display`
* [x] `gdk_cursor_new_from_surface`
	+ [x] use `CairoSurfaceImageT` instead of `CairoSurfaceT`
	+ [x] use `cairo_surface_reference` and `cairo_surface_destroy`
* [x] `gdk_cursor_new_from_name`
* [x] `gdk_cursor_new_for_display`
	+ [x] define
	+ [x] make test
	+ [x] repair test
	+ [x] more cursor types
	+ [x] review
* [x] `gdk_cursor_get_cursor_type`
	+ [x] make a trial

### not now

* `gdk_cursor_new_from_pixbuf`
* `gdk_cusror_get_surface`
* `gdk_cusror_get_image`

### deprecated

* `gdk_cursor_new`
* `gdk_cursor_ref`
* `gdk_cursor_unref`

todo
----

* [x] use `c-enum` in `GdkCursorType`
	+ [x] define `GdkCursorType`
	+ [x] define more members
		- [x] `Gdk_BOTTOM_SIDE` to `GDK_CROSS`
		- [x] `Gdk_CROSS_REVERSE` to `GDK_EXCHANGE`
		- [x] `GDK_FLEUR` to `GDK_HAND2`
		- [x] `GDK_HEART` to `GDK_LEFT_SIDE`
		- [x] `GDK_LEFT_TEE` to `GDK_RIGHT_PTR`
		- [x] `GDK_RIGHT_SIDE` to `GDK_SHUTTLE`
		- [x] `GDK_SIZING` to `GDK_TREK`
		- [x] `GDK_UL_ANGLE` to `GDK_CURSOR_IS_PIXMAP`
* [x] move `GdkCursorType` to here
* [x] check pointer address of GdkCursor to set and GdkCursor to get
* [x] remove `GdkCursorRef`
* [x] move `GdkCursor` to here
* [x] think about `ref` and `unref`
