Tab Stops memo
==============

function
--------

* `pango_tab_array_new`
* `pango_tab_array_new_with_positions`
* `pango_tab_array_copy`
* `pango_tab_array_free`
* `pango_tab_array_get_size`
* `pango_tab_array_resize`
* `pango_tab_array_set_tab`
* `pango_tab_array_get_tab`
* `pango_tab_array_get_tabs`
* `pango_tab_array_get_positions_in_pixels`

todo
----

* [x] `pango_tab_array_new`
* [x] rename `...PangoUnit` to `...Double`
* [x] rename `...PixelUnit` to `...Int`
* [x] `pango_tab_array_get_size`
* [x] `pango_tab_array_resize`
* [x] `pango_tab_array_set_tab`
* [x] repair `pangoTabArrayDoubleSetTab` and `pangoTabArrayIntSetTab`
	+ [50, 150, 200, 250] and set 6 to 550 then you should get
		[50, 150, 200, 250, 350, 450, 550, 600]
* [x] `pangoTabArrayDoubleFreeze`
	+ `:: PangoTabArrayDouble -> m PangoTabArray`
* [x] `pangoTabArrayIntFreeze`
	+ `:: PangoTabArrayInt -> m PangoTabArray`
* [x] `pangoTabArrayThaw`
	+ `:: PangoTabArray -> m (Either PangoTabArrayDouble PangoTabArrayInt)`
* [x] `pangoTabArrayGetTab`
	+ `:: PangoTabArray -> CInt -> Maybe (Either Double Int)`
* [x] `pangoTabArrayGetTabs`
	+ `:: PangoTabArray -> Either [Double] [Int]`
* [x] `pango_tab_array_get_positions_in_pixels`

about memory
------------

* `pango_tab_array_copy`
* `pango_tab_array_free`

not todo yet
------------

not todo
--------

* [ ] `pango_tab_array_new_with_positions`
