PangoLayoutLine memo
====================

function
--------

### get line

* `pango_layout_get_line`
* `pango_layout_get_line_readonly`
* `pango_layout_get_lines`
* `pango_layout_get_lines_readonly`

### process line

* `pango_layout_line_ref`
* `pango_layout_line_unref`
* `pango_layout_line_get_extents`
* `pango_layout_line_get_pixel_extents`
* `pango_layout_line_index_to_x`
* `pango_layout_line_x_to_index`
* `pango_layout_line_get_x_ranges`

todo
----

* [x] move data `Extents` and `PixelExtents` to module `Graphics.Pango.PangoRectangle`
* [ ] move from `Graphics.Pango.Basic.LayoutObjects`
	+ [x] `pangoLayoutGetLine`
		- [x] to check NULL
	+ [x] `pangoLayoutGetLines`
	+ [x] `pangoLayoutLineGetExtents`
	+ [x] `pangoLayoutLineGetPixelExtents`
		- [x] move
		- [x] use `PixelExtents`
	+ [x] `pangoLayoutLineIndexToX`
	+ [x] remove duplicated `boolToGboolean` and `gbooleanAndBool`
	+ [ ] `pangoLayoutLineXToIndex`
	+ [ ] `pangoLayoutLineGetXRanges`
