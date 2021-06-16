GdkDisplay memo
===============

Function
--------

### not deprecated

* `gdk_display_open`
* `gdk_display_get_default`
* `gdk_display_get_name`
* `gdk_display_get_default_screen`
* `gdk_display_device_is_grabbed`
* `gdk_display_keep`				--
* `gdk_display_sync`
* `gdk_display_flush`
* `gdk_display_close`
* `gdk_display_is_closed`
* `gdk_display_get_event`
* `gdk_display_peek_event`
* `gdk_display_put_event`
* `gdk_display_has_pending`
* `gdk_display_set_double_click_time`		--
* `gdk_display_set_double_click_distance`	--
* `gdk_display_supports_cursor_color`
* `gdk_display_supports_cursor_alpha`
* `gdk_display_get_default_cursor_size`
* `gdk_display_get_default_group`		--
* `gdk_display_supports_selection_notification`	--
* `gdk_display_requext_selection_notification`	--
* `gdk_display_supports_clipboard_persistence`	--
* `gdk_display_store_clipboard`			--
* `gdk_display_supports_shapes`			--
* `gdk_display_supports_input_shapes`		--
* `gdk_display_get_app_launch_context`		--
* `gdk_display_notify_startup_complete`		--
* `gdk_display_get_default_seat`
* `gdk_display_list_seats`
* `gdk_display_get_n_monitors`
* `gdk_display_get_monitor`
* `gdk_display_get_primary_monitor`
* `gdk_display_get_monitor_at_point`
* `gdk_display_get_monitor_at_window`

### deprecated

* `gdk_display_get_n_screens`
* `gdk_display_get_screen`
* `gdk_display_get_device_manager`
* `gdk_display_pointer_ungrab`
* `gdk_display_keyboard_ungrab`
* `gdk_display_pointer_is_grabbed`
* `gdk_display_get_pointer`
* `gdk_display_list_devices`
* `gdk_display_get_window_at_pointer`
* `gdk_display_warp_pointer`
* `gdk_display_supports_composite`

### semi-deprecated

* `gdk_display_get_maximal_cursor_size`

now
---

* [x] DISPLAY
	+ [x] `gdk_display_open`
		- [x] process NULL
		- [x] repair: add argument `display_name`
	+ [x] `gdk_display_get_default`
		- [x] process NULL: throw exception
	+ [x] `gdk_display_get_name`
		- this is pure
	+ [x] `gdk_display_get_default_screen`
		- this is pure
	+ [x] `gdk_display_device_is_grabbed`
	+ [x] `gdk_display_sync`
	+ [x] `gdk_display_flush`
	+ [x] `gdk_display_close`
	+ [x] `gdk_display_is_closed`
* [x] EVENT
	+ [x] `gdk_display_get_event`
	+ [x] `gdk_display_peek_event`
	+ [x] `gdk_display_put_event`
	+ [x] `gdk_display_has_pending`
* [x] CURSOR
	+ [x] `gdk_display_supports_cursor_color`
	+ [x] `gdk_display_supports_cursor_alpha`
	+ [x] `gdk_display_get_default_cursor_size`
* [ ] SEAT
	+ [x] `gdk_display_get_default_seat`
	+ [ ] `gdk_display_list_seats`
* [ ] MONITOR
	+ [ ] `gdk_display_get_n_monitors`
	+ [ ] `gdk_display_get_monitor`
	+ [ ] `gdk_display_get_primary_monitor`
	+ [ ] `gdk_display_get_monitor_at_point`
	+ [ ] `gdk_display_get_monitor_at_window`

not now
-------

* `gdk_display_keep`
* `gdk_display_set_double_click_time`
* `gdk_display_set_double_click_distance`
* `gdk_display_get_default_group`
* `gdk_display_supports_selection_notification`
* `gdk_display_requext_selection_notification`
* `gdk_display_supports_clipboard_persistence`
* `gdk_display_store_clipboard`
* `gdk_display_supports_shapes`
* `gdk_display_supports_input_shapes`
* `gdk_display_get_app_launch_context`
* `gdk_display_notify_startup_complete`
