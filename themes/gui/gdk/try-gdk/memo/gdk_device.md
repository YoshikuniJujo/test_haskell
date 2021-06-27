GdkDevice memo
==============

Function
--------

### now

* [x] `gdk_device_get_name`
* [x] `gdk_device_get_vendor_id`
* [x] `gdk_device_get_product_id`
* [x] `gdk_device_get_source`
* [ ] `gdk_device_list_slave_devices`
* [ ] `gdk_device_tool_get_tool_type`

### not now

* `gdk_device_set_mode`
* `gdk_device_get_mode`
* `gdk_device_set_key`
* `gdk_device_get_key`
* `gdk_device_set_axis_use`
* `gdk_device_get_axis_use`
* `gdk_device_get_associated_device`
* `gdk_device_get_device_type`
* `gdk_device_get_display`
* `gdk_device_get_has_cursor`
* `gdk_device_get_n_axes`
* `gdk_device_get_n_keys`
* `gdk_device_get_axes`
* `gdk_device_warp`
* `gdk_device_get_seat`
* `gdk_device_get_state`
* `gdk_device_get_position`
* `gdk_device_get_position_double`
* `gdk_device_get_window_at_position`
* `gdk_device_get_window_at_position_double`
* `gdk_device_get_history`
* `gdk_device_free_history`
* `gdk_device_get_axis`
* `gdk_device_list_axes`
* `gdk_device_get_axis_value`
* `gdk_device_get_last_event_window`
* `gdk_device_tool_get_serial`

### deprecated

* `gdk_device_grab`
* `gdk_device_ungrab`

todo
----

* [x] use c-enum in `GdkInputSource`
* [x] move `GdkInputSource` to the appropriate module
