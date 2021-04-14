Fonts memo
==========

functions
---------

* `pango_font_description_new`
* `pango_font_description_copy`
* `pango_font_description_copy_static`
* `pango_font_description_hash`
* `pango_font_description_equal`
* `pango_font_description_free`
* `pango_font_descriptions_free`
* `pango_font_description_set_family`
* `pango_font_description_set_family_static`
* `pango_font_description_get_family`
* `pango_font_description_set_style`
* `pango_font_description_get_style`
* `pango_font_description_set_variant`
* `pango_font_description_get_variant`
* `pango_font_description_set_weight`
* `pango_font_description_get_weight`
* `pango_font_description_set_stretch`
* `pango_font_description_get_stretch`
* `pango_font_description_set_size`
* `pango_font_description_set_absolute_size`
* `pango_font_description_get_size_is_absolute`
* `pango_font_description_set_gravity`
* `pango_font_description_get_gravity`
* `pango_font_description_set_variations`
* `pango_font_description_set_variations_static`
* `pango_font_description_get_variations`
* `pango_font_description_get_set_fiedlds`
* `pango_font_description_unset_fields`
* `pango_font_description_merge`
* `pango_font_description_merge_static`
* `pango_font_description_better_match`
* `pango_font_description_from_string`
* `pango_font_description_to_string`
* `pango_font_description_to_filename`
* `pango_font_metrics_ref`
* `pango_font_metrics_unref`
* `pango_font_metrics_get_ascent`
* `pango_font_metrics_get_descent`
* `pango_font_metrics_get_approximate_char_width`
* `pango_font_metrics_get_approximate_digit_width`
* `pango_font_metrics_get_underline_thickness`
* `pango_font_metrics_get_underline_position`
* `pango_font_metrics_get_strikethrough_tickness`
* `pango_font_metrics_get_strikethrough_position`
* `PANGO_FONT`
* `PANGO_IS_FONT`
* `pango_font_find_shaper`
* `pango_font_describe`
* `pango_font_describe_with_absolute_size`
* `pango_font_get_coverage`
* `pango_font_get_glyph_extents`
* `pango_font_get_metrics`
* `pango_font_get_font_map`
* `PANGO_FONT_FAMILY`
* `PANGO_IS_FONT_FAMILY`
* `pango_font_family_get_name`
* `pango_font_family_is_monospace`
* `pango_font_family_list_faces`
* `PANGO_FONT_FACE`
* `PANGO_IS_FONT_FACE`
* `pango_font_face_get_face_name`
* `pango_font_face_list_size`
* `pango_font_face_describe`
* `pango_font_face_is_synthesized`
* `PANGO_FONT_MAP`
* `PANGO_IS_FONT_MAP`
* `PANGO_FONT_MAP_CLASS`
* `PANGO_IS_FONT_MAP_CLAWSS`
* `PANGO_FONT_MAP_GET_CLASS`
* `pango_font_map_create_context`
* `pango_font_map_load_font`
* `pango_font_map_load_fontset`
* `pango_font_map_list_families`
* `pango_font_map_get_shape_engine_type`
* `pango_font_map_get_serial`
* `pango_font_map_changed`
* `pango_fontset_get_font`
* `pango_fontset_get_metrics`
* `pango_fontset_foreach`
* `pango_fontset_simple_new`
* `pango_fontset_simple_append`
* `pango_fontset_simple_size`

todo
----

* [x] use `PangoDescriptionPrim (PrimState m)`
	+ [x] comment out `pangoFontDescriptionEqual`
	+ [x] `pangoFontDescriptionGetFamily`
	+ [x] `pangoFontDescriptionGetStyle`
	+ [x] `pangoFontDescriptionGetVariant`
	+ [x] `pangoFontDescriptionGetWeight`
	+ [x] `pangoFontDescriptionGetStretch`
	+ [x] `pangoFontDescriptionGetSize`
	+ [x] `pangoFontDescriptionGetSizeIsAbsolute`
	+ [x] `pangoFontDescriptionGetGravity`
	+ [x] `pangoFontDescriptionGetSetField`
	+ [x] `pangoFontDescriptionToString`
	+ [x] `pangoFontDescriptionToFilename`
* [x] remove `PangoDescription`
* [x] try to remove `PangoContextOld`
* [x] rename `PangoDescriptionPrim (PrimState m)` to `PangoDescription (PrimState m)`
* [ ] use `unsafeIOToPrim` instead of `unprimIo`
	+ [x] `pangoFontDescriptionNew`
	+ [ ] `pangoFontDescriptionCopy`
	+ [ ] others
* [ ] remove `unprimIo`
* [ ] `pango_font_description_new`
* [ ] `pango_font_description_copy`
* [ ] `pango_font_description_equal`
* [ ] `pango_font_description_free`
* [ ] `pango_font_description_set_foo and pango_font_description_get_foo`
	+ [ ] family
	+ [ ] style
	+ [ ] variant
	+ [ ] weight
	+ [ ] stretch
	+ [ ] size
	+ [ ] gravity
	+ [ ] variations
* [ ] `pango_font_description_set_absolute_size`
* [ ] `pango_font_description_get_size_is_absolute`
* [ ] `pango_font_description_get_set_fields`
* [ ] `pango_font_description_unset_fields`
* [ ] `pango_font_description_merge`
* [ ] `pango_font_description_better_match`
* [ ] `pango_font_description_from_string`
* [ ] `pango_font_description_to_string`
* [ ] `pango_font_description_to_filename`
* [ ] `pango_font_metrics_get_foo`
	+ [ ] `ascent`
	+ [ ] `descent`
	+ [ ] `approximate_char_width`
	+ [ ] `approximate_digit_width`
	+ [ ] `underline_thickness`
	+ [ ] `underline_position`
	+ [ ] `strikethrough_thickness`
	+ [ ] `strikethrough_position`
* [ ] `pango_font_find_shaper`
* [ ] `pango_font_describe`
* [ ] `pango_font_describe_with_absolute_size`
* [ ] `pango_font_get_foo`
	+ [ ] `coverage`
	+ [ ] `glyph_extents`
	+ [ ] `metrics`
	+ [ ] `font_map`
* [ ] `pango_font_family_get_name`
* [ ] `pango_font_family_is_monospace`
* [ ] `pango_font_face_get_face_name`
* [ ] `pango_font_face_list_sizes`
* [ ] `pango_font_face_describe`

not todo yet
------------

* `pango_font_description_copy_static`
* `pango_font_description_hash`
* `pango_font_descriptions_free`
* `pango_font_description_set_family_static`
* `pango_font_description_set_variations_static`
* `pango_font_description_merge_static`
* `PANGO_FONT`
* `PANGO_IS_FONT`
* `PANGO_FONT_FAMILY`
* `PANGO_IS_FONT_FAMILY`
* `PANGO_FONT_FACE`
* `PANGO_IS_FONT_FACE`
* `PANGO_FONT_MAP`
* `PANGO_IS_FONT_MAP`
* `PANGO_FONT_MAP_CLASS`
* `PANGO_IS_FONT_MAP_CLASS`
* `PANGO_FONT_MAP_GET_CLASS`
* `pango_font_map_create_context`
* `pango_font_map_load_font`
* `pango_font_map_load_fontset`
* `pango_font_map_list_families`
* `pango_font_map_get_shape_engine_type`
* `pango_font_map_get_serial`
* `pango_font_map_changed`
* `pango_fontset_get_font`
* `pango_fontset_get_metrics`
* `pango_fontset_foreach`
* `pango_fontset_simple_new`
* `pango_fontset_simple_append`
* `pango_fontset_simple_size`

for GC
------

* `pango_font_metrics_ref`
* `pango_font_metrics_unref`
