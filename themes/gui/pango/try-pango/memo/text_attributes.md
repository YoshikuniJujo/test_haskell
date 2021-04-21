Text Attributes memo
====================

function
--------

* `pango_parse_markup`
* `pango_markup_parser_new`
* `pango_markup_parser_finish`
* `pango_attr_type_register`
* `pango_attr_type_get_name`
* `pango_attribute_init`
* `pango_attribute_copy`
* `pango_attribute_equal`
* `pango_attribute_destroy`
* `pango_attr_language_new`
* `pango_attr_family_new`
* `pango_attr_style_new`
* `pango_attr_variant_new`
* `pango_attr_stretch_new`
* `pango_attr_weight_new`
* `pango_attr_size_new`
* `pango_attr_size_new_absolute`
* `pango_attr_font_desc_new`
* `pango_attr_foreground_new`
* `pango_attr_background_new`
* `pango_attr_strikethrough_new`
* `pango_attr_strikethrough_color_new`
* `pango_attr_underline_new`
* `pango_attr_underline_color_new`
* `pango_attr_shape_new`
* `pango_attr_shape_new_with_data`
* `pango_attr_scale_new`
* `pango_attr_rise_new`
* `pango_attr_letter_spacing_new`
* `pango_attr_fallback_new`
* `pango_attr_gravity_new`
* `pango_attr_gravity_hint_new`
* `pango_attr_font_features_new`
* `pango_attr_foreground_alpha_new`
* `pango_attr_background_alpha_new`
* `pango_color_parse`
* `pango_color_copy`
* `pango_color_free`
* `pango_color_to_string`
* `pango_attr_list_new`
* `pango_attr_list_ref`
* `pango_attr_list_unref`
* `pango_attr_list_copy`
* `pango_attr_list_insert`
* `pango_attr_list_insert_before`
* `pango_attr_list_change`
* `pango_attr_list_splice`
* `pango_attr_list_filter`
* `pango_attr_list_get_iterator`
* `pango_atrr_iterator_copy`
* `pango_attr_iterator_next`
* `pango_attr_iterator_range`
* `pango_attr_iterator_get`
* `pango_attr_iterator_get_font`
* `pango_attr_iterator_get_attrs`
* `pango_attr_iterator_destroy`

todo
----

* [x] `pango_parse_markup`
	+ [x] use `Maybe Char`
	+ [x] rename `try-pango-attrs` to `try-pango-attrs-markup`
	+ [x] `try-pango-attrs-markup` use a command line argument
	+ [x] use `pattern GErrorMarkup`
* [x] `pango_markup_parser_new`
* [x] `pango_markup_parser_finish`
* [x] edit `app/try-pango-attrs-markup-stream`
	+ use `[T.Text]` instead of `T.Text`
* [x] `g_markup_parse_context_free`
* [x] `g_markup_parse_context_parse`
* [ ] `pango_attr_type_get_name`
* [ ] `pango_attribute_copy`
* [ ] `pango_attribute_destroy`
* [ ] `pango_attr_foo_new`
	+ [ ] language
	+ [ ] family
	+ [ ] style
	+ [ ] variant
	+ [ ] stretch
	+ [ ] weight
	+ [ ] size
	+ [ ] size absolute
	+ [ ] font description
	+ [ ] foreground
	+ [ ] background
	+ [ ] strikethrough
	+ [ ] strikethrough color
	+ [ ] underline
	+ [ ] underline color
	+ [ ] shape
	+ [ ] shape with data
	+ [ ] scale
	+ [ ] rise
	+ [ ] letter spacing
	+ [ ] fallback
	+ [ ] gravity
	+ [ ] gravity hint
	+ [ ] font features
	+ [ ] foreground alpha
	+ [ ] background alpha
* [ ] `pango_color_parse`
* [ ] `pango_color_to_string`
* [ ] `pango_attr_list_new`
* [ ] `pango_attr_list_unref`
* [ ] `pango_attr_list_copy`
* [ ] `pango_attr_list_insert`
* [ ] `pango_attr_list_insert_before`
* [ ] `pango_attr_list_change`

not todo yet
------------

* `pango_attr_type_register`
* `pango_attribute_init`
* `pango_attribute_equal`
* `pango_color_parse`
* `pango_color_free`
* `pango_attr_list_ref`
* `pango_attr_list_splice`
* `pango_attr_list_filter`
* `pango_attr_list_get_iterator`
* `pango_attr_iterator_copy`
* `pango_attr_iterator_next`
* `pango_attr_iterator_range`
* `pango_attr_iterator_get`
* `pango_attr_iterator_get_font`
* `pango_attr_iterator_get_attrs`
* `pango_attr_iterator_destroy`
