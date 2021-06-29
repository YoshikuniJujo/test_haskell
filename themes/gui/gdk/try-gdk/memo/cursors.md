Cursors memo
============

Function
--------

### now

* [ ] `gdk_cursor_new_from_surface`
* [ ] `gdk_cursor_new_from_name`
* [ ] `gdk_cursor_new_for_display`
	+ [x] define
	+ [ ] make test
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
* [ ] move `GdkCursorType` to here
