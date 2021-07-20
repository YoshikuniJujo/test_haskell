Events memo
===========

Function
--------

### todo

* [ ] refactor expor list
* [ ] remove
	+ [x] gdkEventCopy
	+ [x] gdkEventNew
	+ [x] gdkEventPut
	+ [ ] more

### now

* [x] `gdk_event_pending`
* [x] `gdk_event_peek`
* [x] `gdk_event_get`
* [x] `gdk_event_put`
* [x] `gdk_event_new`
	+ `gdkWithEventNew`
* [x] `gdk_event_copy`
	+ `gdkWithEventCopy`
* [x] `gdk_event_free`
* [x] `gdk_get_show_events`
* [x] `gdk_set_show_events`
* [ ] `gdk_event_get_device_tool`
	+ add to motion event structure

### not now

* [x] `gdk_event_get_axis`
	+ remove it
* [x] `gdk_event_get_button`
	+ [x] make `GdkEventButton`
		- [x] define `GdkEventButtonRaw`
		- [x] define `GdkEventSealedGdkButtonPress`
		- [x] define `GdkEventButton`
		- [x] define `gdkEventButton`
	+ [x] try `GdkEventButton`
	+ [x] remove it
* [x] `gdk_event_get_click_count`
	+ remove it
* [x] `gdk_event_get_coords`
	+ remove it
* [x] `gdk_event_get_keycode`
	+ [x] check keycode
	+ [x] check keycode and scancode
	+ [x] remove it
* [x] `gdk_event_get_keyval`
	+ remove it
* [x] `gdk_event_get_root_coords`
	+ remove it
* [x] `gdk_event_get_scroll_direction`
	+ [x] try to get scroll direction
	+ [x] remove it
* [x] `gdk_event_get_scroll_deltas`
	+ [x] from `delta_x` and `delta_y` to `Maybe (delta_x, delta_y)`
	+ [x] remove it
* [x] `gdk_event_is_scroll_stop_event`
	+ remove it
* [x] `gdk_event_get_state`
	+ remove it
* [x] `gdk_event_get_time`
	+ [x] check `GDK_CURRENT_TIME`
	+ [x] remove it
* [x] `gdk_event_get_window`
	+ [x] rename `GdkEventSealedGdkEventAny` to `GdkEventSealedGdkAny`
	+ [x] `GdkEventAny`
	+ [x] `gdkEventAny`
	+ [x] remove it
* [x] `gdk_event_get_event_type`
	+ remove it
* `gdk_event_get_event_sequence`
* `gdk_event_request_motions`
* `gdk_event_get_angle`
* `gdk_event_get_center`
* `gdk_event_get_distance`
* `gdk_event_triggers_context_menu`
* `gdk_event_get_seat`
* [x] `gdk_event_get_scancode`
	+ remove it
* `gdk_event_get_pointer_emulated`
* `gdk_event_handler_set`
* [x] `gdk_event_set_screen`
	+ remove it
* [x] `gdk_event_get_screen`
	+ remove it
* [x] `gdk_event_get_device`
	+ remove it
* [x] `gdk_event_set_device`
	+ remove it
* [x] `gdk_event_get_source_device`
	+ remove it
* [x] `gdk_event_set_source_device`
	+ remove it
* `gdk_event_set_device_tool`
* `gdk_setting_get`

EventStructures
---------------

* [x] add an export list
* [x] move GdkEvent structures to `Graphics.Gdk.EventStructures`
	+ [x] `GdkEventAny`
	+ [x] `GdkEventKey`
	+ [x] `GdkEventMotion`
	+ [x] `GdkEventVisibility`
	+ [x] `GdkEventFocus`
	+ [x] `GdkEventConfigure`
	+ [x] `GdkEventWindowState`
	+ [x] about `GdkEventAny`
* [x] move enums to `Graphics.Gdk.EventStructures`
	+ [x] GdkScrollDirection
		- [x] use `c-enum`
		- [x] move to `Graphics.Gdk.EventStructures`
	+ [x] GdkVisibilityState
		- [x] use `c-enum`
	+ [x] GdkWindowState
* [x] define `newtype GdkEventAnyRaw`
* [x] define `pattern GdkEventSealedGdkEventAny`
* [x] use `GdkEventAny` to get type
* [x] remove first argument of `GdkEventSealed`
* [x] `GdkEventScroll`
	+ [x] `GdkEventScrollRaw`
	+ [x] `GdkEventSealedGdkEventScroll`
	+ [x] `GdkEventScroll`
	+ [x] `gdkEventScroll`
* [x] define `GdkEventSealedGdkFoo`
	+ [x] Nothing
	+ [x] Delete
	+ [x] KeyRelease
	+ [x] FocusChange
	+ [x] Map
	+ [x] Unmap
	+ [x] Configure
	+ [x] VisibilityNotify
	+ [x] WindowState
* [ ] define `GdkFoo` and converter
	+ [ ] Configure
	+ [ ] Visibility
	+ [ ] WindowState
* [x] use `gdkWithEvent`
	+ [x] Main
	+ [x] simple
	+ [x] test-cursor
	+ [x] try-cairo-tribial
	+ [x] try-cursor-name
	+ [x] try-cursor-type
	+ [x] try-decorations
	+ [x] try-display
	+ [x] try-draw
	+ [x] try-key-press
	+ [x] try-memory-bug-fix
	+ [x] try-memory-bug
	+ [x] try-motion
	+ [x] try-multidevice
	+ [x] try-tribial
	+ [x] try-window-type-hint
* [x] remove `gdkEventGet`
* [ ] remove `GdkEvent` and so on
	+ [ ] `Graphics.Gdk.Events`: use `GdkEventSealed`
	+ [ ] remove `GdkEvent`
* [ ] use `c-struct` in `GdkEventFoo`
	+ [x] `GdkEventAny`
	+ [x] `GdkEventKey`
		- [x] `type`
		- [x] `window`
		- [x] `send_event`
		- [x] `time`
		- [x] `state`
			* [x] define GdkModifierType
			* [x] add `state`
		- [x] `keyval`
		- [x] `lengthDeprecated`
		- [x] `stringDeprecated`
		- [x] `hardware_keycode`
		- [x] `group`
		- [x] `is_modifier`
	+ [x] check `gdk_event_copy`
	+ [x] make module `Graphics.Gdk.GdkDevice.Axes`
	+ [x] define type `GdkAxes`
	+ [x] rename `GdkEventMotion` to `GdkEventMotionRaw`
	+ [x] rename `GdkEventGdkEventMotion` to `GdkEventGdkEventMotionRaw`
	+ [x] `GdkEventMotionRaw`
		- [x] `type`
		- [x] `window`
		- [x] `send_event`
		- [x] `time`
		- [x] `x`
		- [x] `y`
		- [x] `axes`
		- [x] `state`
		- [x] `is_hint`
		- [x] `device`
		- [x] `x_root`
		- [x] `y_root`
	+ [x] `gdkAxesCopy :: GdkDevice -> GdkAxes -> IO GdkAxes`
	+ [x] `GdkEventMotion`
	+ [x] `gdkEventMotion :: Sealed s GdkEventMotionRaw -> GdkEventMotion`
	+ [ ] remove `tryGdkEventMotionCopy`
	+ [x] `GdkEventVisibility`
	+ [ ] `GdkEventFocus`
	+ [x] `GdkEventConfigure`
	+ [x] `GdkEventWindowState`
* [x] try to peek and poke bit field by capi
* [x] remove `deriving Num` from `GdkKeySym`

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

Dessin
------

### GdkEventMotion

* rename `GdkEvenMotion` to `GdkEventMotionRaw`
* rename `GdkEventGdkEventMotion` to `GdkEventGdkEventMotionRaw`
* define `GdkEventMotionNoAxes`
* define as `data GdkEventMotion = GdkEventMotion GdkEventMotionNoAxes GdkAxes`
* define converter from `GdkEventMotionRaw` to `GdkEventMotion`
* define pattern `GdkEventGdkEventMotion`
* use `GdkEventGdkEventMotion` instead of `GdkEventGdkEventMotionRaw`
* remove `GdkEventGdkEventMotionRaw`
