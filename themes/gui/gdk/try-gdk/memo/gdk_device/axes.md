GdkDevice axes memo
===================

todo
----

* [x] add export list
* [x] consider to make Graphics.Gdk.GdkDevice.GdkAxes.Internal
* [ ] refactor expor list
	+ [x] GDK AXES
	+ [ ] DEVICE
		- [ ] `gdkDeviceSetAxisUse`
		- [x] `gdkDeviceGetAxisUse`
		- [x] `gdkDeviceGetNAxes`
		- [x] `gdkDeviceGetAxes`
		- [ ] `gdkDeviceGetAxis`
		- [ ] `gdkDeviceListAxes`
		- [ ] `gdkDeviceGetAxisValue`
	+ [ ] GDK AXIS USE
	+ [ ] GDK AXIS FLAGS
		- [ ] Multiple Flags
		- [ ] Single Flags

Function
--------

* [x] define `GdkAxes`
* [x] `gdk_device_set_axis_use`
* [x] `gdk_device_get_axis_use`		Since: 2.20
	+ [x] define `GdkAxisUse`
	+ [x] define `gdkDeviceGetAxisUse`
* [x] `gdk_device_get_n_axes`		Since: 3.0
* [x] `gdk_device_get_axes`		Since: 3.22
	+ [x] define `GdkAxisFlag`
	+ [x] define `GdkAxisFlags`
	+ [x] define `gdkAxisFlags`
	+ [x] define `gdkAxisFlagList`
	+ [x] define `gdkDeviceGetAxes`
* [x] `gdk_device_get_axis`
* [x] `gdk_device_list_axes`		Since: 3.0
* [x] `gdk_device_get_axis_value`	Since: 3.0
