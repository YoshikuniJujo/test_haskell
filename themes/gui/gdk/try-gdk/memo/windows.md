Windows memo
============

* [x] refactor export list some
* [x] make `Graphics.Gdk.Windows.Internal`
* [x] add options to reduce outputs
	+ [x] `--display-screen-etc`
	+ [x] `--show-device`
* [x] refactor export list
	+ [x] structure
	+ [x] GDK WINDOW AND GDK WINDOW AUTO UNREF
	+ [x] NEW, DESTROY AND SHOW
		- [x] `gdkWindowNew`
			* [x] try `Nothing`
			* [x] try `Just foo`
		- [x] `gdkWindowDestroy`
		- [x] `gdkWindowShow`
		- [x] `gdkWindowShowUnraised`
		- [x] `gdkWindowHide`
	+ [x] DISPLAY, SCREEN, VISUAL AND WINDOW
		- [x] `gdkWindowGetDisplay`
		- [x] `gdkWindowGetScreen`
		- [x] `gdkWindowGetVisual`
		- [x] `gdkGetDefaultRootWindow`
		- [x] `gdkWindowGetToplevel`
		- [x] `gdkWindowGetParent`
		- [x] `gdkWindowReparent`
			* [x] make child window
			* [x] reparent
		- [x] `gdkWindowPeekChildren`
	+ [x] EVENT
		- [x] Event Mask
			* [x] `gdkWindowGetEvents`
			* [x] `gdkWindowSetEvents`
			* [x] `gdkWindowGetDeviceEvents`
				+ [x] get pointer device
				+ [x] check pointer device event mask
				+ [x] get keyboard device
				+ [x] consider to remove it
			* [x] `gdkWindowSetDeviceEvents`
				+ [x] add main loop
				+ [x] use `GetOpt`
				+ [x] check event mask of master device
					- [x] pointer device
				+ [x] check event mask of physical device
					- [x] pointer device
				+ [x] consider to remove it
			* [x] `gdkWindowGetSourceEvents`
				+ consider to remove it
			* [x] `gdkWindowSetSourceEvents`
				+ consider to remove it
		- [x] Event Compression
			* [x] `gdkWindowGetEventCompression`
			* [x] `gdkWindowSetEventCompression`
	+ [x] TITLE AND CURSOR
		- [x] `gdkWindowSetTitle`
		- [x] `gdkWindowSetCursor`
		- [x] `gdkWindowGetCursor`
		- [x] `gdkWindowGetDeviceCursor`
			* consider to remove it
		- [x] `gdkWindowSetDeviceCursor`
			* consider to remove it
	+ [x] MULTIPLE DEVICE
		- consider to remove it
	+ [x] WITH DRAW FRAME
		- [x] `gdkWindowWithDrawFrame`
		- [x] `gdkWindowGetVisibleRegion`
	+ [x] WINDOW TYPE
		- `gdkWindowGetWindowType`
	+ [x] IS DESTROYED, VISIBLE, VIEWABLE, INPUT ONLY OR SHAPED
		- [x] `gdkWindowIsDestroyed`
		- [x] `gdkWindowIsVisible`
		- [x] `gdkWindowIsViewable`
		- [x] `gdkWindowIsInputOnly`
		- [x] `gdkWindowIsShaped`
	+ [x] GDK WINDOW STATES
		- [x] `gdkWindowGetState`
		- [x] `gdkWindowWithdraw`
		- [x] `gdkWindowIconify`
		- [x] `gdkWindowDeiconify`
		- [x] `gdkWindowStick`
		- [x] `gdkWindowUnstick`
		- [x] `gdkWindowMaximize`
		- [x] `gdkWindowUnmaximize`
		- [x] `gdkWindowFullscreen`
		- [x] `gdkWindowUnfullscreen`
		- [x] Gdk Fullscreen Mode
			* [x] `gdkWindowGetFullscreenMode`
			* [x] `gdkWindowSetFullscreenMode`
			* [x] `data GdkFullscreenMode`
	+ [x] GEOMETRY AND OPACITY
		- [x] structure
			* [x] classify z and xy
			* [x] others
		- [x] Z Axis
			* [x] `gdkWindowSetKeepAbove`
			* [x] `gdkWindowSetKeepBelow`
			* [x] `gdkWindowRaise`
			* [x] `gdkWindowLower`
			* [x] `gdkWindowFocus`
				+ [x] second argument to `MilliSecond`
				+ [x] others
			* [x] `gdkWindowSetOpacity`
		- [x] XY Axis
			* [x] get position and size
				+ [x] window
					- [x] `gdkWindowGetGeometry`
					- [x] `gdkWindowGetPosition`
					- [x] `gdkWindowGetWideh`
					- [x] `gdkWindowGetHeight`
					- [x] `gdkWindowGetOrigin`
					- [x] `gdkWindowGetRootCoords`
				+ [x] frame
					- [x] `gdkWindowGetFrameExtents`
					- [x] `gdkWindowGetRootOrigin`
			* [x] move and resize
				- [x] `gdkWindowMoveResize`
				- [x] `gdkWindowMove`
				- [x] `gdkWindowResize`
	+ [x] WINDOW BEHAVIER AND APPEARANCE
		- [x] Pass Through
			* [x] `gdkWindowSetPassThrough`
				+ consider to remove it
			* [x] `gdkWindowGetPassThrough`
				+ consider to remove it
		- [x] Transient For
			* `gdkWindowSetTransientFor`
		- [x] Modal Hint
			* [x] `gdkWindowSetModalHint`
			* [x] `gdkWindowGetModalHint`
		- [x] Window Type Hint
		- [x] Task Bar, Pager and Urgency
			* [x] `gdkWindowSetSkipTaskbarHint`
			* [x] `gdkWindowSetSkipPagerHint`
			* [x] `gdkWindowSetUrgencyHint`
		- [x] Gdk Wm Decoration
			* [x] structure
			* [x] `gdkWindowSetDecorations`
			* [x] `gdkWindowGetDecorations`
			* [x] `GdkWindowDecorations`
				+ [x] type
				+ [x] function
					- `gdkWMDecorations`
			* [x] `GdkWindowDecoration`
				+ [x] type
				+ [x] function
					- `gdkWMDecorationList`
				+ [x] patterns
					- [x] `pattern GdkDecorAll`
					- [x] `pattern GdkDecorBorder`
					- [x] `pattern GdkDecorResizeh`
					- [x] `pattern GdkDecorTitle`
					- [x] `pattern GdkDecorMenu`
					- [x] `pattern GdkDecorMinimize`
					- [x] `pattern GdkDecorMaximize`

Function
--------

### now

* [x] `gdk_window_new`
* [x] `gdk_window_destroy`
* [x] `gdk_window_get_window_type`
* [x] `gdk_window_get_display`
* [x] `gdk_window_get_screen`
* [x] `gdk_window_get_visual`
	+ [x] make a trial
* [x] `gdk_window_show`
* [x] `gdk_window_show_unraised`
* [x] `gdk_window_hide`
	+ [x] make a trial
* [x] `gdk_window_is_destroyed`
* [x] `gdk_window_is_visible`
* [x] `gdk_window_is_viewable`
* [x] `gdk_window_is_input_only`
* [x] `gdk_window_is_shaped`
* [ ] `gdk_window_get_state`
	+ [ ] make a trial
* [x] `gdk_window_withdraw`
* [x] `gdk_window_iconify`
* [x] `gdk_window_deiconify`
* [x] `gdk_window_stick`
* [x] `gdk_window_unstick`
* [x] `gdk_window_maximize`
* [x] `gdk_window_unmaximize`
* [x] `gdk_window_fullscreen`
* [x] `gdk_window_unfullscreen`
* [x] `gdk_window_get_fullscreen_mode`
* [x] `gdk_window_set_fullscreen_mode`
* [x] `gdk_window_set_keep_above`
* [x] `gdk_window_set_keep_below`
* [x] `gdk_window_set_opacity`
* [x] `gdk_window_set_pass_through`
* [x] `gdk_window_get_pass_through`
* [x] `gdk_window_move`
* [x] `gdk_window_resize`
* [x] `gdk_window_move_resize`
* [x] `gdk_window_reparent`
* [x] `gdk_window_raise`
* [x] `gdk_window_lower`
* [x] `gdk_window_focus`
* [x] `gdk_window_begin_draw_frame`
* [x] `gdk_window_end_draw_frame`
* [x] `gdk_window_get_visible_region`
* [x] `gdk_window_set_title`
* [x] `gdk_window_set_cursor`
* [x] `gdk_window_get_cursor`
* [x] `gdk_window_get_geometry`
* [x] `gdk_window_get_width`
* [x] `gdk_window_get_height`
* [x] `gdk_window_set_modal_hint`
* [x] `gdk_window_get_modal_hint`
* [x] `gdk_window_set_type_hint`
* [x] `gdk_window_get_type_hint`
* [x] `gdk_window_set_skip_taskbar_hint`
* [x] `gdk_window_set_skip_pager_hint`
* [x] `gdk_window_set_urgency_hint`
* [x] `gdk_window_get_position`
* [x] `gdk_window_get_root_origin`
* [x] `gdk_window_get_frame_extents`
* [x] `gdk_window_get_origin`
* [x] `gdk_window_get_root_coords`
* [x] `gdk_window_get_parent`
* [x] `gdk_window_get_toplevel`
* [x] `gdk_window_peek_children`
* [x] `gdk_window_get_events`
* [x] `gdk_window_set_events`
* [x] `gdk_window_set_transient_for`
* [x] `gdk_window_set_decorations`
	+ [x] define `GdkWMDecoration`
	+ [x] define `newtype GdkWMDecorations`
	+ [x] define `gdkWMDecorations :: [GdkWMDecoration] -> GdkWMDecorations`
	+ [x] define `gdkWindowSetDecorations`
* [x] `gdk_window_get_decorations`
	+ [x] define `gdkWMDecorationList :: GdkWMDecorations -> [GdkWMDecoration]`
	+ [x] define `gdkWindowGetDecorations`
* [x] `gdk_get_default_root_window`
* [x] `gdk_window_get_support_multidevice`
* [x] `gdk_window_set_support_multidevice`
* [x] `gdk_window_get_device_cursor`
	+ [x] foreign import
	+ [x] define `gdkWindowGetDeviceCursor`
* [x] `gdk_window_set_device_cursor`
	+ [x] get seat
	+ [x] get devices
	+ [x] get device sources
	+ [x] make cursor
	+ [x] define `gdkWindowSetDeviceCursor`
	+ [x] repair `gdkWindowSetDeviceCursor`
		- [x] repair `gdkWindowDestroy`
* [x] `gdk_window_get_device_events`
	+ [x] foreign import
	+ [x] define `gdkWindowGetDeviceEvents`
* [x] `gdk_window_set_device_events`
* [x] `gdk_window_get_source_events`
* [x] `gdk_window_set_source_events`
* [x] `gdk_window_get_event_compression`
* [x] `gdk_window_set_event_compression`

### not now

* `gdk_window_fullscreen_on_monitor`
* `gdk_window_scroll`
* `gdk_window_move_to_rect`
* `gdk_window_move_region`
* `gdk_window_has_native`
* `gdk_window_ensure_native`
* `gdk_window_restack`
* `gdk_window_register_dnd`
* `gdk_window_begin_resize_drag`
* `gdk_window_begin_resize_drag_for_device`
* `gdk_window_begin_move_drag`
* `gdk_window_begin_move_drag_for_device`
* `gdk_window_show_window_menu`
* `gdk_window_constraint_size`
* `gdk_window_beep`
* `gdk_window_get_scale_factor`
* `gdk_window_set_opaque_region`
* `gdk_window_create_gl_context`
* `gdk_window_mark_paint_from_clip`
* `gdk_window_get_clip_region`
* `gdk_window_set_invalidate_handler`
* [x] `gdk_window_invalidate_rect`
	+ remove it
* `gdk_window_invalidate_region`
* `gdk_window_invalidate_maybe_recurse`
* `gdk_window_get_update_area`
* [x] `gdk_window_freeze_updates`
	+ remove it
* [x] `gdk_window_thaw_updates`
	+ remove it
* `gdk_window_get_frame_clock`
* `gdk_window_set_user_data`
* [x] `gdk_window_set_override_redirect`
	+ remove it
* [x] `gdk_window_set_accept_focus`
	+ remove it
* [x] `gdk_window_get_accept_focus`
	+ remove it
* [x] `gdk_window_set_focus_on_map`
	+ remove it
* `gdk_window_get_focus_on_map`
* `gdk_window_add_filter`
* `gdk_window_remove_filter`
* `gdk_window_shape_combine_region`
* `gdk_window_set_child_shapes`
* `gdk_window_merge_child_shapes`
* `gdk_window_input_shape_combine_region`
* `gdk_window_set_child_input_shapes`
* `gdk_window_merge_child_input_shapes`
* `gdk_window_get_user_data`
* `gdk_window_set_geometry_hints`
* `gdk_window_set_icon_list`
* `gdk_window_set_shadow_width`
* `gdk_window_get_device_position`
* `gdk_window_get_device_posittion_double`
* `gdk_window_get_children`
* `gdk_window_get_children_with_user_data`
* [x] `gdk_window_set_icon_name`
	+ remove it
* `gdk_window_set_role`
* `gdk_window_set_startup_id`
* `gdk_window_set_group`
* `gdk_window_get_group`
* `gdk_window_set_functions`
* `gdk_offscreen_window_get_surface`
* `gdk_offscreen_window_set_embedder`
* `gdk_offscreen_window_get_embedder`
* `gdk_window_geometry_changed`
* `gdk_window_coords_from_parent`
* `gdk_window_corrds_to_parent`
* `gDk_window_get_effective_parent`
* `gdk_window_get_effective_toplevel`

### deprecated

* `gdk_window_at_pointer`
* `gdk_window_set_composited`
* `gdk_window_get_compisited`
* `gdk_window_flush`
* `gdk_window_begin_paint_rect`
* `gdk_window_begin_paint_region`
* `gdk_window_end_paint`
* `gdk_window_process_all_updates`
* `gdk_window_process_updates`
* `gdk_window_set_debug_updates`
* `gdk_window_enable_synchronized_configure`
* `gdk_window_configure_finished`
* `gdk_window_set_static_gravities`
* `gdk_window_set_background`
* `gdk_window_set_background_rgba`
* `gdk_window_set_background_pattern`
* `gdk_window_get_background_pattern`
* `gdk_window_get_pointer`

todo
----

* [x] use `c-enum` in `GdkWindowAttributesType`
* [x] move `GdkWindowAttributesType` to the appropriate module
* [x] make `Graphics.Gdk.Windows.GdkWindowAttr`
* [x] use `g_object_ref` at gdkWindowNew
* [x] use `g_object_unref` at gdkWindowDestroy
* [x] use `g_object_unref` at gdkWindowSetCursor
* [x] use `g_object_ref` at gdkWindowSetCursor
* [x] process NULL in `gdk_window_get_cursor`
* [x] give an argument `s` to `GdkDrawingContext`
* [x] move `GdkDrawingContext s` to module `Graphics.Gdk.GdkDrawingContext`
* [x] get contents of CairoRegionT
* [x] `GdkEventMask`
	- [x] use `c-enum`
		* [x] `GdkEventMaskSignleBit`
		* [x] `GdkEventMaskMultiBits` (only `GDK_ALL_EVENTS_MASK`)
	- [x] define `gdkEventMaskMultiBits ::`
		`[GdkEventMaskSingleBit] -> GdkEventMaskMultiBits`
	- [x] define `GdkEventMaskSingleBit` and `GdkEventMaskMultiBits`
	- [x] use `GdkEventMaskMultiBits` in `minimalWindowAttr`
	- [x] move `separateBits` to `Data.Bits.Misc`
	- [x] define `gdkEventMaskSingleBitList ::`
		`GdkEventMaskMultiBits -> [GdkEventMaskSingleBit]`
	- [x] use `GdkEventMaskMultiBits` in `gdkWindowGetEvents`
	- [x] use `GdkEventMaskMultiBits` in `gdkWindowSetEvents`
	- [x] move to the appropriate module
