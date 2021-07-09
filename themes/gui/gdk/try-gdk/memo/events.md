Events memo
===========

* [x] add an export list
* [ ] move GdkEvent structures to Graphics.Gdk.EventStructures
	+ [x] `GdkEventAny`
	+ [x] `GdkEventKey`
	+ [x] `GdkEventMotion`
	+ [x] `GdkEventVisibility`
	+ [x] `GdkEventFocus`
	+ [ ] `GdkEventConfigure`
	+ [ ] `GdkEventProperty`
	+ [ ] `GdkEventSelection`
	+ [ ] `GdkEventDND`
	+ [ ] `GdkEventProximity`
	+ [ ] `GdkEventWindowState`
	+ [ ] `GdkEventSetting`
	+ [ ] `GdkEventOwnerChange`
	+ [ ] more
	+ [ ] about `GdkEventAny`
* [ ] use `c-struct` in `GdkEventFoo`

Function
--------

* `gdk_events_pending`
* `gdk_event_peek`
* `gdk_event_get`
* `gdk_event_put`
* `gdk_event_new`
* `gdk_event_copy`
* `gdk_event_free`
* `gdk_event_get_axis`
* `gdk_event_get_button`
* `gdk_event_get_keycode`
* `gdk_event_get_keyval`
* `gdk_event_get_root_coords`
* `gdk_event_get_scroll_direction`
* `gdk_event_is_scroll_stop_event`
* `gdk_event_get_state`
* `gdk_event_get_time`
* `gdk_event_get_window`
* `gdk_event_get_event_type`
* `gdk_event_get_event_sequence`
* `gdk_event_request_motions`
* `gdk_event_get_angle`
* `gdk_event_get_center`
* `gdk_event_get_distance`
* `gdk_event_triggers_context_menu`
* `gdk_event_get_seat`
* `gdk_event_get_scancode`
* `gdk_event_get_pointer_enulated`
* `gdk_event_handler_set`
* `gdk_get_show_events`
* `gdk_set_show_events`
* `gdk_event_set_screen`
* `gdk_event_get_screen`
* `gdk_event_get_device`
* `gdk_event_set_device`
* `gdk_event_get_source_device`
* `gdk_event_set_source_device`
* `gdk_event_get_device_tool`
* `gdk_event_set_device_tool`
* `gdk_setting_get`
