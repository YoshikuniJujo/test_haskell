Windows memo
============

Function
--------

### now

* [ ] `gdk_window_new`
* [ ] `gdk_window_destroy`
* [ ] `gdk_window_get_window_type`
* [ ] `gdk_window_get_display`
* [ ] `gdk_window_get_screen`
* [ ] `gdk_window_get_visual`
* [ ] `gdk_window_show`
* [ ] `gdk_window_show_unraised`
* [ ] `gdk_window_hide`
* [ ] `gdk_window_is_destroyed`
* [ ] `gdk_window_is_visible`
* [ ] `gdk_window_is_viewable`
* [ ] `gdk_window_is_input_only`
* [ ] `gdk_window_is_shaped`
* [ ] `gdk_window_get_state`
* [ ] `gdk_window_withdraw`
* [ ] `gdk_window_iconify`
* [ ] `gdk_window_maximize`
* [ ] `gdk_window_fullscreen`
* [ ] `gdk_window_set_opacity`
* [ ] `gdk_window_freeze_updates`
* [ ] `gdk_window_thaw_updates`
* [ ] `gdk_window_invalidate_rect`
* [ ] `gdk_window_set_events`
* [ ] `gdk_window_set_title`
* [ ] `gdk_window_set_cursor`
* [ ] `gdk_window_get_cursor`
* [ ] `gdk_window_get_width`
* [ ] `gdk_window_get_height`
* [ ] `gdk_window_get_position`
* [ ] `gdk_window_get_parent`
* [ ] `gdk_window_get_decorations`
* [ ] `gdk_get_default_root_window`
* [ ] `gdk_window_set_device_cursor`
* [ ] `gdk_window_set_event_compression`

### not now

* `gdk_window_deiconify`
* `gdk_window_stick`
* `gdk_window_unstick`
* `gdk_window_unmaximize`
* `gdk_window_fullscreen_on_monitor`
* `gdk_window_unfullscreen`
* `gdk_window_get_fullscreen_mode`
* `gdk_window_set_fullscreen_mode`
* `gdk_window_set_keep_above`
* `gdk_window_set_keep_below`
* `gdk_window_set_pass_through`
* `gdk_window_get_pass_through`
* `gdk_window_move`
* `gdk_window_resize`
* `gdk_window_move_resize`
* `gdk_window_scroll`
* `gdk_window_move_to_rect`
* `gdk_window_move_region`
* `gdk_window_has_native`
* `gdk_window_ensure_native`
* `gdk_window_reparent`
* `gdk_window_raise`
* `gdk_window_lower`
* `gdk_window_restack`
* `gdk_window_focus`
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
* `gdk_window_begin_draw_frame`
* `gdk_window_end_draw_frame`
* `gdk_window_get_visible_region`
* `gdk_window_set_invalidate_handler`
* `gdk_window_invalidate_region`
* `gdk_window_invalidate_maybe_recurse`
* `gdk_window_get_update_area`
* `gdk_window_get_frame_clock`
* `gdk_window_set_user_data`
* `gdk_window_set_override_redirect`
* `gdk_window_set_accept_focus`
* `gdk_window_get_accept_focus`
* `gdk_window_set_focus_on_map`
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
* `gdk_window_get_geometry`
* `gdk_window_set_geometry_hints`
* `gdk_window_set_icon_list`
* `gdk_window_set_modal_hint`
* `gdk_window_get_modal_hint`
* `gdk_window_set_type_hint`
* `gdk_window_get_type_hint`
* `gdk_window_set_shadow_width`
* `gdk_window_set_skip_taskbar_hint`
* `gdk_window_set_skip_pager_hint`
* `gdk_window_set_urgency_hint`
* `gdk_window_get_root_origin`
* `gdk_window_get_frame_extents`
* `gdk_window_get_origin`
* `gdk_window_get_root_coords`
* `gdk_window_get_device_position`
* `gdk_window_get_device_posittion_double`
* `gdk_window_get_toplevel`
* `gdk_window_get_children`
* `gdk_window_get_children_with_user_data`
* `gdk_window_peek_children`
* `gdk_window_get_events`
* `gdk_window_set_icon_name`
* `gdk_window_set_transient_for`
* `gdk_window_set_role`
* `gdk_window_set_startup_id`
* `gdk_window_set_group`
* `gdk_window_get_group`
* `gdk_window_set_decorations`
* `gdk_window_set_functions`
* `gdk_window_get_support_multidevice`
* `gdk_window_set_support_multidevice`
* `gdk_window_get_device_cursor`
* `gdk_window_get_device_events`
* `gdk_window_set_device_events`
* `gdk_window_get_event_compression`
* `gdk_offscreen_window_get_surface`
* `gdk_offscreen_window_set_embedder`
* `gdk_offscreen_window_get_embedder`
* `gdk_window_geometry_changed`
* `gdk_window_coords_from_parent`
* `gdk_window_corrds_to_parent`
* `gdk_window_get_effective_parent`
* `gdk_window_get_effective_toplevel`
* `gdk_window_set_device_cursor`

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
* [ ] move `GdkWindowAttributesType` to the appropriate module
