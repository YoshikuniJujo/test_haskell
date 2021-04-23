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
* [x] define `PangoAttribute s`
	+ [x] define type
	+ [x] define `mkPangoAttributePrim`
		- [x] `pango_attribute_destroy`
* [x] `pangoAttributeSetStartIndex`
	+ `:: PrimMonad m => PangoAttribhute (PrimState m) -> CUInt -> m ()`
* [x] `pangoAttributeSetEndIndex`
	+ `:: PrimMonad m => PangoAttribhute (PrimState m) -> CUInt -> m ()`
* [x] `pango_attribute_copy`
* [ ] `pango_attr_foo_new`
	+ [x] language
	+ [x] family
	+ [x] style
	+ [x] variant
	+ [x] stretch
	+ [x] weight
	+ [x] size
	+ [x] size absolute
	+ [x] font description
	+ [x] foreground
	+ [x] background
	+ [x] strikethrough
	+ [x] strikethrough color
	+ [x] underline
	+ [x] underline color
	+ [x] shape
	+ [x] scale
	+ [x] rise
	+ [x] letter spacing
	+ [x] fallback
	+ [ ] gravity
		- [ ] process about `PANGO_GRAVITY_AUTO`
	+ [x] gravity hint
	+ [x] font features
	+ [ ] foreground alpha
	+ [ ] background alpha
* [ ] `pango_color_parse`
* [ ] `pango_color_to_string`
* [x] `pango_attr_list_new`
* [x] `pango_attr_list_unref`
* [x] `pango_attr_list_copy`
	+ [x] as `pangoAttrListFreeze`
	+ [x] as `pangoAttrListThaw`
* [x] `pango_attr_list_insert`
* [x] `pango_attr_list_insert_before`
* [ ] `pango_attr_list_change`
* [ ] define `pangoAttributeCopy`
	+ [ ] define
	+ [ ] use in `app/try-pango-attrs.hs`

not todo yet
------------

* `pango_attr_type_register`
* `pango_attr_type_get_name`
* `pango_attribute_init`
* `pango_attribute_equal`
* `pango_attr_shape_new_with_data`
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
