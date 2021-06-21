GdkScreen memo
==============

Function
--------

### not deprecated

* `gdk_screen_get_default`
* `gdk_screen_get_system_visual`
* `gdk_screen_get_rgba_visual`
* `gdk_screen_is_composited`
* `gdk_screen_get_root_window`
* `gdk_screen_get_display`
* `gdk_screen_list_visuals`
* `gdk_screen_get_toplevel_windows`
* `gdk_screen_get_setting`
* `gdk_screen_get_font_options`
* `gdk_screen_set_font_options`
* `gdk_screen_get_resolution`
* `gdk_screen_set_resolution`
* `gdk_screen_get_window_stack`

### deprecated

* `gdk_screen_get_number`
* `gdk_screen_get_width`
* `gdk_screen_get_height`
* `gdk_screen_get_width_mm`
* `gdk_screen_get_height_mm`
* `gdk_screen_make_display_name`
* `gdk_screen_get_n_monitors`
* `gdk_screen_get_primary_monitor`
* `gdk_screen_get_monitor_geometry`
* `gdk_screen_get_monitor_workarea`
* `gdk_screen_get_monitor_at_point`
* `gdk_screen_get_monitor_at_window`
* `gdk_screen_get_monitor_height_mm`
* `gdk_screen_get_monitor_width_mm`
* `gdk_screen_get_monitor_plug_name`
* `gdk_screen_get_monitor_scale_factor`
* `gdk_screen_get_active_window`

### now

* [x] `gdk_screen_get_default`
	+ [x] check NULL
* [x] `gdk_screen_get_system_visual`
* [x] `gdk_screen_get_rgba_visual`
* [x] `gdk_screen_is_composited`
* [ ] `gdk_screen_get_root_window`
* [ ] `gdk_screen_get_display`
* [ ] `gdk_screen_list_visuals`
* [ ] `gdk_screen_get_toplevel_windows`
* [ ] `gdk_screen_get_resolution`
* [ ] `gdk_screen_get_window_stack`

### not now

* [ ] `gdk_screen_get_setting`
* [ ] `gdk_screen_get_font_options`
* [ ] `gdk_screen_set_font_options`
* [ ] `gdk_screen_set_resolution`

todo
----

* [x] use c-enum in `GdkWindowType`
* [x] use c-enum in `GdkWindowState`
* [x] move `GdkWindowType` to the appropriate module.
* [x] move `GdkWindowState` to the appropriate module.
