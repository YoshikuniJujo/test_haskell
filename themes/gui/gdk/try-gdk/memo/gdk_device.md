GdkDevice memo
==============

* [x] make class IsGdkDevice
* [x] make GdkDevice
* [x] make GdkDeviceMaster and GdkDevicePhysical
* [x] consider separate GdkDeviceMaster to
	+ GdkDeviceMasterPointer
	+ GdkDeviceMasterKeyboard
	+ consder to make class IsGdkDeviceMaster
* [x] make `SomeGdkDevice` system
	+ [x] define `SomeGdkDevice`
	+ [x] define `SomeGdkDeviceMaster`
	+ [x] refactor about it
	+ [x] refactor export list
* [ ] repair `Graphics.Gdk.GdkDevice`
	+ [ ] gdkDisplayDeviceIsGrabbed
* [ ] repair `Graphics.Gdk.GdkSeat`
	+ [ ] `gdkSeatGetPointer`
	+ [ ] `gdkSeatGetKeyboard`
	+ [ ] `gdkSeatGetSlaves`
* [ ] repair `Graphics.Gdk.GdkDevice`
	+ [ ] `gdkDeviceGetDisplay`
	+ [ ] `gdkDeviceGetSeat`
	+ [ ] `gdkDeviceGetName`
	+ [ ] `gdkDeviceGetDeviceType`
	+ [ ] `gdkDeviceGetSource`
	+ [ ] `gdkDeviceGetVendorId`
	+ [ ] `gdkDeviceGetProductId`
	+ [ ] `gdkDeviceListSlaveDevices`
	+ [ ] GEOMETRY
		+ [ ] `gdkDeviceWarp`
			- [x] try it with master
			- [x] try it with physical
		+ [ ] `gdkDeviceGetPosition`
		+ [ ] `gdkDeviceGetPositionDouble`
		+ [ ] `gdkDeviceGetWindowAtPosition`
		+ [ ] `gdkDeviceGetWindowAtPositionDouble`
	+ [ ] STATE
		+ [ ] `gdkDeviceGetHasCursor`
		+ [ ] `gdkDeviceGetLastEventWindow`
* [ ] repair `Graphics.Gdk.GdkDevice.GdkAxes`
	+ [ ] `gdkDeviceSetAxisUse`
	+ [ ] `gdkDeviceGetAxisUse`
	+ [ ] `gdkDeviceGetNAxes`
	+ [ ] `gdkDeviceGetAxes`
	+ [ ] `gdkDeviceGetAxis`
	+ [ ] `gdkDeviceListAxes`
	+ [ ] `gdkDeviceGetAxisValue`
	+ [ ] `gdkAxesCopyFromPtr`

refactor export list
--------------------

* [x] consider to remove GdkDeviceTool
* [x] refactor structure
* [x] make `Graphics.Gdk.GdkDevice.Internal`
* [x] GDK DEVICE
* [x] DISPLAY AND SEAT
	+ [x] `gdkDeviceGetDisplay`
	+ [x] `gdkDeviceGetSeat`
* [ ] IDENTITY
	+ [x] rearrange
	+ [x] `gdkDeviceGetName`
	+ [ ] `gdkDeviceGetDeviceType`
	+ [ ] `gdkDeviceGetSource`
	+ [ ] `gdkDeviceGetVenderId`
	+ [ ] `gdkDeviceGetProductId`
* [x] SLAVES
	+ `gdkDeviceListSlaveDevices`
	+ [x] consider whether or not to use maybe value
	+ [x] others
* [ ] GEOMETRY
* [ ] STATE
* [ ] GDK DEVICE TYPE
* [ ] GDK INPUT SOURCE

Function
--------

### now

* [x] `gdk_device_get_name`
* [x] `gdk_device_get_vendor_id`
* [x] `gdk_device_get_product_id`
* [x] `gdk_device_get_source`
* [x] `gdk_device_list_slave_devices`
* [x] `gdk_device_tool_get_tool_type`
* [x] `gdk_device_get_device_type`
* [x] `gdk_device_get_display`
* [x] `gdk_device_get_has_cursor`
* [x] `gdk_device_warp`
* [x] `gdk_device_get_seat`
* [x] `gdk_device_get_position`
* [x] `gdk_device_get_position_double`
* [x] `gdk_device_get_window_at_position`
* [x] `gdk_device_get_window_at_position_double`
* [x] `gdk_device_get_last_event_window`

### not to do for a while

* `gdk_device_set_mode`
* `gdk_device_get_mode`
* `gdk_device_set_key`
* `gdk_device_get_key`
* `gdk_device_get_associated_device`
* `gdk_device_get_n_keys`
* `gdk_device_get_state`
* `gdk_device_get_history`
* `gdk_device_free_history`
* `gdk_device_tool_get_serial`

### axes

* [x] `gdk_device_set_axis_use`
* [x] `gdk_device_get_axis_use`
* [x] `gdk_device_get_n_axes`
* [x] `gdk_device_get_axes`
* [x] `gdk_device_get_axis`
* [x] `gdk_device_list_axes`
* [x] `gdk_device_get_axis_value`

### deprecated

* `gdk_device_grab`
* `gdk_device_ungrab`

todo
----

* [x] use c-enum in `GdkInputSource`
* [x] move `GdkInputSource` to the appropriate module
* [x] use `Ptr` instead of `ForeignPtr` in `GdkDevice`
* [x] move `GdkDevice` to the appropriate module
* [x] use c-enum in `GdkDeviceToolType`
* [x] move `GdkDeviceToolType` to the appropriate module
* [x] use `Ptr` instead of `ForeignPtr` in `GdkDeviceTool`
* [x] move `GdkDeviceTool` to the appropriate module
