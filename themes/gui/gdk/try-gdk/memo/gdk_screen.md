GdkScreen memo
==============

todo
----

* [x] make `Graphics.Gdk.GdkScreen.Internal`
* [ ] refactor export list
	+ [x] TYPE
	+ [x] DEFAULT SCREEN
	+ [x] VISUAL
		- [x] `gdkScreenGetSystemVisual`
		- [x] `gdkScreenGetRgbaVisual`
		- [x] `gdkScreenListVisuals`
	+ [x] IS COMPOSITED
		- `gdkScreenIsComposited`
	+ [ ] WINDOW
		- [ ] `gdkScreenGetRootWindow`
		- [ ] `gdkScreenGetToplevelWindows`
		- [ ] `gdkScreenGetWindowStack`
			* [x] about `GdkWindowAutoUnref`
	+ [ ] DISPLAY
		- `gdkScreenGetDisplay`
	+ [ ] RESOLUTION
		- [ ] `gdkScreenGetResolution`
		- [ ] `gdkScreenSetResolution`

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
* [x] `gdk_screen_get_root_window`
* [x] `gdk_screen_get_display`
* [x] `gdk_screen_list_visuals`
* [x] `gdk_screen_get_toplevel_windows`
* [x] `gdk_screen_get_resolution`
* [x] `gdk_screen_set_resolution`
* [x] `gdk_screen_get_window_stack`
	+ [x] need `g_object_unref` for returned windows

### not now

* [x] `gdk_screen_get_setting`
* [x] `gdk_screen_get_font_options`
* [x] `gdk_screen_set_font_options`

todo
----

* [x] use c-enum in `GdkWindowType`
* [x] use c-enum in `GdkWindowState`
* [x] move `GdkWindowType` to the appropriate module
* [x] move `GdkWindowState` to the appropriate module
* [x] use c-enum in `GdkVisualType`
* [x] move `GdkVisualType` to the appropriate module
