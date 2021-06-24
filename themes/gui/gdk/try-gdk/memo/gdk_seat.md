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
