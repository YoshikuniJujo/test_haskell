Events memo
===========

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
* [ ] define `GdkEventSealedGdkFoo`
	+ [x] Nothing
	+ [x] Delete
	+ [x] KeyRelease
	+ [x] FocusChange
	+ [x] Map
	+ [x] Unmap
	+ [x] Configure
	+ [x] VisibilityNotify
	+ [ ] WindowState
* [ ] define `GdkFoo` and converter
	+ [ ] Configure
	+ [ ] Visibility
	+ [ ] WindowState
* [ ] use `gdkWithEvent`
	+ [ ] Main
	+ [ ] simple
	+ [ ] test-cursor
	+ [ ] more
* [ ] remove `gdkEventGet`
* [ ] remove `GdkEvent` and so on
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
	+ [ ] `GdkEventWindowState`
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
