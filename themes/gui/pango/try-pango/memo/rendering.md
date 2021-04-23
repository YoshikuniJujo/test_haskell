Rendering memo
==============

function
--------

* `pango_itemize`
* `pango_itemize_with_base_dir`
* `pango_item_free`
* `pango_item_new`
* `pango_item_split`
* `pango_reorder_items`
* `pango_context_new`
* `pango_context_changed`
* `pango_context_get_serial`
* `pango_context_set_font_map`
* `pango_context_get_font_map`
* `pango_context_get_font_description`
* `pango_context_set_font_description`
* `pango_context_get_language`
* `pango_context_set_language`
* `pango_context_get_base_dir`
* `pango_context_set_base_dir`
* `pango_context_get_base_gravity`
* `pango_context_set_base_gravity`
* `pango_context_get_gravity`
* `pango_context_get_gravity_hint`
* `pango_context_set_gravity_hint`
* `pango_context_get_matrix`
* `pango_context_set_matrix`
* `pango_context_load_font`
* `pango_context_load_fontset`
* `pango_context_get_metrics`
* `pango_context_list_families`
* `pango_break`
* `pango_get_log_attrs`
* `pango_find_paragraph_boundary`
* `pango_default_break`
* `pango_shape`
* `pango_shape_full`

todo
----

* [x] make a module `Graphics.Pango.Basic.Rendering`
* [x] define `PangoContext s`
* [x] rename `pangoContext` to `PangoContextOld`
* [x] use `PangoContext s` from `pangoCairoCreateContext`
* [x] define `class PangoContextSetting`
* [x] `pango_context_set_base_gravity`
* [x] `pango_context_get_base_gravity`
* [ ] `pango_context_get_gravity`
* [x] `pango_context_set_font_description`
* [x] `pango_context_get_font_description`
