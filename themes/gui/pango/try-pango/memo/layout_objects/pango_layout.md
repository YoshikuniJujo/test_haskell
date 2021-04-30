PangoLayout memo
================

function
--------

* `pango_layout_new`
* `pango_layout_copy`
* `pango_layout_get_context`
* `pango_layout_context_changed`
* `pango_layout_get_serial`
* `pango_layout_set_text`
* `pango_layout_get_text`
* `pango_layout_get_character_count`
* `pango_layout_set_markup`
* `pango_layout_set_markup_with_accel`
* `pango_layout_set_attributes`
* `pango_layout_get_attributes`
* `pango_layout_set_font_description`
* `pango_layout_get_font_description`
* `pango_layout_set_width`
* `pango_layout_get_width`
* `pango_layout_set_height`
* `pango_layout_get_height`
* `pango_layout_set_ellipsize`
* `pango_layout_get_ellipsize`
* `pango_layout_is_ellipsize`
* `pango_layout_set_indent`
* `pango_layout_get_indent`
* `pango_layout_get_spacing`
* `pango_layout_set_spacing`
* `pango_layout_set_justify`
* `pango_layout_get_justify`
* `pango_layout_set_auto_dir`
* `pango_layout_get_auto_dir`
* `pango_layout_set_alignment`
* `pango_layout_get_alignment`
* `pango_layout_set_tabs`
* `pango_layout_get_tabs`
* `pango_layout_set_single_paragraph_mode`
* `pango_layout_get_single_paragraph_mode`
* `pango_layout_get_unknown_glyphs_count`
* `pango_layout_get_log_attrs`
* `pango_layout_get_log_attrs_readonly`
* `pango_layout_index_to_pos`
* `pango_layout_index_to_line_x`
* `pango_layout_xy_to_index`
* `pango_layout_get_cursor_pos`
* `pango_layout_move_cursor_visually`
* `pango_layout_get_extents`
* `pango_layout_get_pixel_extents`
* `pango_layout_get_size`
* `pango_layout_get_pixel_size`
* `pango_layout_get_baseline`

todo
----

* [x] make module `Graphics.Pango.Basic.LayoutObjects.PangoLayoutIo`
* [x] move `PangoLayoutIo` to it
* [x] make module `Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim`
* [x] define `PangoLayoutPrim s`
* [x] make `pangoLayoutNew` to return `PangoLayoutPrim (PrimState m)`
* [x] `pango_layout_new`
* [x] make `pangoLayoutSetFontDescription` to use `PangoFontDescriptionPrim (PrimState m)` as argument
* [x] remove `PangoFontDescription`
* [x] rename `PangoFontDescriptionPrim (PrimState m)` to `PangoFontDescription (PrimState m)`
* [x] use `IO` instead of `PrimMonad m => m`
	+ [x] pangoLayoutSetIndent
	+ [x] pangoLayoutSetAlignment
	+ [x] pangoLayoutSetTabs
	+ [x] pangoLayoutSetSingleParagraphMode
* [x] change from `PangoLayoutPrim s` to `PangoLayoutPrim`
* [x] remove `PangoLayout`
* [x] rename `PangoLayoutPrim` to `PangoLayout`
* [x] use `IO` instead of pure: pangoLayoutGetFoo
* [x] move functions to `Graphics.Pango.Basic.LayoutObjects.PangoLayout`
* [x] define `class PangoLayoutSetting`
* [x] `pangoLayoutSet` and `pangoLayoutGet`
	+ [x] `Text`
		- [x] use `Text` instead of `String`
		- [x] check Null
	+ [x] `PangoAttrList`
		- [x] check Null
	+ [x] `PangoFontDescription`
		- [x] check Null
	+ [x] `Width`
		- [x] check Null
		- [x] to reset
	+ [x] `Height`
		- [x] check Null
		- [x] to reset
	+ [x] `PangoWrapMode`
	+ [x] `PangoEllipsizeMode`
	+ [x] `Indent`
	+ [x] `Spacing`
	+ [x] `Justify`
	+ [x] `AutoDir`
	+ [x] `PangoAlignment`
	+ [x] `PangoTabArray`
		- [x] define
		- [x] check Null
	+ [x] `SingleParagraphMode`
	+ [x] `PangoTextAttrList`
* [x] instances of `class PangoLayoutInfo`
	+ [x] try to define about `CharacterCount`
	+ [x] `pango_layout_get_character_count`
	+ [x] `pango_layout_is_ellipsize`
	+ [x] `pango_layout_is_wrapped`
	+ [x] `pango_layout_get_unknown_glyphs_count`
	+ [x] `pango_layout_get_log_attrs`
		- [x] `pangoLayoutGetLogAttrs`
		- [x] `pangoLogAttrsGetLogAttr :: PangoLogAttrs -> Int -> PangoLogAttr`
		- [x] more infomations in a trial
		- [x] `instance PangoLayoutInfo`
	+ [x] `pango_layout_get_extents`
		- [x] covert from pango unit
	+ [x] `pango_layout_get_pixel_extents`
	+ [x] `pango_layout_get_size`
	+ [x] `pango_layout_get_pixel_size`
	+ [x] `pango_layout_get_baseline`
	+ [x] `pango_layout_get_line_count`
* [x] `pango_layout_index_to_pos`
	+ [x] use indices of UTF-8
* [x] `pango_layout_index_to_line_x`
* [x] `pango_layout_xy_to_index`
* [ ] `pango_layout_get_cursor_pos`
* [ ] `pango_layout_move_cursor_visually`
* [x] `pango_layout_set_markup`
* [x] `pango_layout_set_markup_with_accel`

not todo yet
------------

* `pango_layout_copy`
* `pango_layout_get_context`
* `pango_layout_context_changed`
* `pango_layout_get_serial`
* `pango_layout_get_log_attrs_readonly`
