PangoMatrix memo
================

functions
---------

* `pango_matrix_copy`
* `pango_matrix_free`
* `pango_matrix_translate`
* `pango_matrix_scale`
* `pango_matrix_rotate`
* `pango_matrix_concat`
* `pango_matrix_transform_point`
* `pango_matrix_transform_distance`
* `pango_matrix_transform_rectangle`
* `pango_matrix_transform_pixel_rectangle`
* `pango_matrix_get_font_scale_factor`
* `pango_matrix_get_font_scale_factors`

todo
----

* [x] `newtype PangoMatrix`
* [x] `pattern PangoMatrix`
* [x] `instance Show PangoMatrix`
* [x] `pango_matrix_copy`
	+ [x] `pango_matrix_free`
	+ [x] `pangoMatrixFreeze`
	+ [x] `pangoMatrixThaw`
	+ [x] `pangoMatrixCopy`
* [x] `pango_matrix_translate`
* [x] `pango_matrix_scale`
* [x] `pango_matrix_rotate`
	+ [x] define Angle which has Degree and Radian
	+ [x] use Angle
* [x] `pango_matrix_concat`
* [x] `pango_matrix_transform_point`
* [x] `pango_matrix_transform_distance`
* [ ] `pango_matrix_transform_rectangle`
* [ ] `pango_matrix_transform_pixel_rectangle`
* [ ] `pango_matrix_get_font_scale_factor`
* [ ] `pango_matrix_get_font_scale_factors`
* [ ] use template for `PangoMatrix`
	+ [x] `newtype PangoMatrix`
	+ [ ] `pattern PangoMatrix`
		- [ ] function `pangoMatrix`
		- [ ] others
	+ [ ] `instance Show`
	+ [ ] `instance Read`
* [ ] use template for `PangoMatrixPrim`
	+ [ ] `newtype PangoMatrixPrim s`
	+ [ ] function `pangoMatrixFreeze`
	+ [ ] function `pangoMatrixThaw`
	+ [ ] function `pangoMatrixCopy`
