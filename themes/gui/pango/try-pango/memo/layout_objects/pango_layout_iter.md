PangoLayoutIter memo
====================

function
--------

* `pango_layout_get_iter`
* `pango_layout_iter_copy`
* `pango_layout_iter_free`
* `pango_layout_iter_next_run`
* `pango_layout_iter_next_char`
* `pango_layout_iter_next_cluster`
* `pango_layout_iter_next_line`
* `pango_layout_iter_at_last_line`
* `pango_layout_iter_get_index`
* `pango_layout_iter_get_baseline`
* `pango_layout_iter_get_run`
* `pango_layout_iter_get_run_readonly`
* `pango_layout_iter_get_line`
* `pango_layout_iter_get_line_readonly`
* `pango_layout_iter_get_layout`
* `pango_layout_iter_get_char_extents`
* `pango_layout_iter_get_cluster_extents`
* `pango_layout_iter_get_run_extents`
* `pango_layout_iter_get_line_yrange`
* `pango_layout_iter_get_line_extents`
* `pango_layout_iter_get_layout_extents`

todo
----

* [x] make module `Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter`
* [x] repair about `PangoLayoutIter`
* [x] move functions to this module
	+ [x] pangoLayoutGetIter
	+ [x] pangoLayoutIterNextRun
	+ [x] pangoLayoutIterNextChar
	+ [x] pangoLayoutIterNextCluster
	+ [x] pangoLayoutIterNextLine
	+ [x] pangoLayoutIterAtLastLine
	+ [x] pangoLayoutIterGetIndex
	+ [x] pangoLayoutIterGetBaseline
	+ [x] pangoLayoutIterGetRun
	+ [x] pangoLayoutIterGetLine
	+ [x] remove pangoLayoutIterGetLayout
	+ [x] remove pangoLayoutIterGetLayoutExtents
	+ [x] pangoLayoutIterGetCharExtents
	+ [x] pangoLayoutIterGetClusterExtents
	+ [x] pangoLayoutIterGetRunExtents
	+ [x] pangoLayoutIterGetLineYrange
	+ [x] pangoLayoutIterGetLineExtents
* [x] repair about GC
	+ [x] pangoLayoutGetIter
* [x] `PangoLayoutIter s`
* [x] pangoLayoutWithIter
