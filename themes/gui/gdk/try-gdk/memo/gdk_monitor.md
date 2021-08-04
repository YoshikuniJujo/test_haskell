GdkMonitor memo
================

refactor export list
--------------------

* [x] refactor structure
* [x] make `Graphics.Gdk.GdkMonitor.Internal`
* [x] GDK MONITOR
* [x] DISPLAY
	+ `gdkMonitorGetDisplay`
* [x] GEOMETRY
	+ [x] `gdkMonitorGetGeometory`
	+ [x] `gdkMonitorGetWorkarea`
	+ [x] `gdkMonitorGetWidthMm`
	+ [x] `gdkMonitorGetHeightMm`
* [x] MANUFACTURER AND MODEL
	+ [x] `gdkMonitorGetManufacturer`
	+ [x] `gdkMonitorGetModel`
* [x] PROPERTIES
	+ [x] `gdkMonitorGetScaleFactor`
	+ [x] `gdkMonitorGetRefreshRate`
	+ [x] `gdkMonitorGetSubpixelLayout`
* [x] IS PRIMARY
	+ `gdkMonitorIsPrimary`
* [x] GDK SUBPIXEL LAYOUT
	+ enum `GdkSubpixelLayout`

review
------

* [x] `gdkMonitorGetDisplay`
* [x] `gdkMonitorGetGeometry`
* [x] `gdkMonitorGetWorkarea`
* [x] `gdkMonitorGetWidthMm`
* [x] `gdkMonitorGetHeightMm`
* [x] `gdkMonitorGetManufacturer`
* [x] `gdkMonitorGetModel`
* [x] `gdkMonitorGetScaleFactor`
* [x] `gdkMonitorGetRefreshRate`
* [x] `gdkMonitorGetSubpixelLayout`
* [x] `gdkMonitorIsPrimary`

todo
----

* [x] try to use `c-struct` in type `GdkRectangle`
* [x] move `GdkRectangle` to the appropriate module
* [x] use `c-enum` in type `GdkSubpixelLayout`
* [x] move `GdkSubpixelLayout` to the appropriate module
