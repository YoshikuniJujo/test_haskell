GdkDevice axes memo
===================

todo
----

* [x] add export list
* [x] consider to make Graphics.Gdk.GdkDevice.GdkAxes.Internal
* [x] refactor expor list
	+ [x] GDK AXES
	+ [x] NUMBER AND AVAILABLE
		- [x] `gdkDeviceGetNAxes`
		- [x] `gdkDeviceGetAxes`
	+ [x] WITH AXIS USE
		- [x] `gdkDeviceSetAxisUse`
		- [x] `gdkDeviceGetAxisUse`
		- [x] `gdkDeviceGetAxis`
			* [x] GdkAxisX and GdkAxisY
			* [x] GdkAxisPressure
			* [x] GdkAxisXtilt and GdkAxisYtilt
			* [x] GdkAxisWheel
			* [x] GdkAxisDistance
			* [x] GdkAxisRotation
			* [x] GdkAxisSlider
	+ [x] WITH ATOMS
		- [x] `gdkDeviceListAxes`
		- [x] `gdkDeviceGetAxisValue`
	+ [x] GDK AXIS USE
	+ [x] GDK AXIS FLAGS
		- [x] Multiple Flags
		- [x] Single Flags

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
