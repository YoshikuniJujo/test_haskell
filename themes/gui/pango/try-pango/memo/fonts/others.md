Fonts others memo
=================

* PangoFontMetrics
* PangoFont
* PangoFontFamily
* PangoFontFace	
* PangoFontMap
* PangoFontset

functions
---------

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
