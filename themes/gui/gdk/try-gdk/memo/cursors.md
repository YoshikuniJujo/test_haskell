Cursors memo
============

Function
--------

### now

* [ ] `gdk_cursor_new_from_surface`
* [ ] `gdk_cursor_new_from_name`
* [ ] `gdk_cursor_new_for_display`
	+ [x] define
	+ [x] make test
	+ [x] repair test
	+ [ ] more cursor types
* [ ] `gdk_cursor_get_display`
* [ ] `gdk_cusror_get_surface`
* [ ] `gdk_cursor_get_cursor_type`

### not now

* `gdk_cursor_new_from_pixbuf`
* `gdk_cusror_get_image`

### deprecated

* `gdk_cursor_new`
* `gdk_cursor_ref`
* `gdk_cursor_unref`

todo
----

* [ ] use `c-enum` in `GdkCursorType`
	+ [x] define `GdkCursorType`
	+ [ ] define more members
		- [x] `Gdk_BOTTOM_SIDE` to `GDK_CROSS`
		- [x] `Gdk_CROSS_REVERSE` to `GDK_EXCHANGE`
		- [x] `GDK_FLEUR` to `GDK_HAND2`
		- [ ] `GDK_HEART` to `GDK_LEFT_SIDE`
		- [ ] `GDK_LEFT_TEE` to `GDK_RIGHT_PTR`
		- [ ] `GDK_RIGHT_SIDE` to `GDK_SHUTTLE`
		- [ ] `GDK_SIZING` to `GDK_TREK`
		- [ ] `GDK_UL_ANGLE` to `GDK_CURSOR_IS_PIXMAP`
* [ ] move `GdkCursorType` to here
* [ ] move `GdkCursor` to here
* [ ] think about `ref` and `unref`
