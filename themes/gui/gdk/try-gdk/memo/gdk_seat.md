GdkSeat memo
============

todo
----

* [x] use c-enum in `GdkSeatCapabilities`
* [x] move `GdkSeatCapabilities` to the appropriate module
* [x] define `GdkSeatCapability` and `GdkSeatCapabilities`
* [x] move `GdkSeat` from `Types`
* [x] use `GdkSeat` instead of `Ptr GdkSeat`
* [x] define `gdkSeatUngrab`
* [x] use `boolToGboolean` of `System.GLib.Bool`
* [x] review export list
* [x] repair `toPtr`
	+ [x] define `withGdkSeatGrabPrepareFunc`
	+ [x] use `withGdkSeatGrabPrepareFunc`
	+ [x] define `withPtr`
	+ [x] use `withPtr`
	+ [x] repair `class Pointerable`
* [x] move `class Pointerable` to package `glib-stopgap`
* [x] make `Graphics.Gdk.GdkSeat.Internal`
* [ ] refactor export list
	+ [x] structure
	+ [x] GDK SEAT
	+ [x] GET
		- [x] `gdkSeatGetDisplay`
		- [x] `gdkSeatGetCapabilities`
		- [x] `gdkSeatGetPointer`
		- [x] `gdkSeatGetKeyboard`
		- [x] `gdkSeatGetSlaves`
			* [x] return value: whether or not to use maybe value
			* [x] others
	+ [ ] GRAB
		- [ ] `gdkSeatGrab`
			* [x] try `owner_events` parameter
				+ [x] make 2 window
				+ [x] 'o': owner_events is True
				+ [x] check event's window
			* [x] try `cursor` parameter
			* [ ] try `event` parameter
			* [ ] try `prepare_func` parameter
		- [x] `gdkSeatGrabSimple`
			* [x] create window
			* [x] use mainLoop
			* [x] try to grab
		- [x] `gdkSeatUngrab`
		- [ ] `GdkSeatGrabPrepareFunc`
	+ [ ] GDK SEAT CAPABILITIES
		- [ ] `GdkSeatCapabilities`
		- [ ] `gdkSeatCapabilities`
		- [ ] `GdkSeatCapability`
		- [ ] `gdkSeatCapabilityList`
