Points and Rectangles memo
==========================

todo new
--------

* [x] make `Graphics.Gdk.PointsAndRectangles.Internal`
* [x] refactor export list
	+ [x] GDK RECTANGLE
		- [x] `data GdkRectangle`
		- [x] `pattern GdkRectangle`
		- [x] `gdkRectangleX`
		- [x] `gdkRectangleY`
		- [x] `gdkRectangleWidth`
		- [x] `gdkRectangleHeight`
	+ [x] GDK RECTANGLE PRIM
		- [x] `data GdkRectanglePrim`
		- [x] `type GdkRectangleIO`
		- [x] `type GdkRectangleST`
		- [x] `gdkRectangleNew`
		- [x] `gdkRectangleFreeze`
		- [x] `gdkRectangleThaw`
		- [x] `gdkRectangleCopy`
	+ [x] FUNCTION
		- [x] `gdkRectangleIntersect`
		- [x] `gdkRectangleUnion`
		- [x] `gdkRectangleEqual`

Function
--------

* `gdk_rectangle_intersect`
* `gdk_rectangle_union`
* `gdk_rectangle_equal`

Type
----

* `GdkPoint`
* `GdkRectangle`

todo
----

* [ ] use `c-struct` in `GdkPoint`
* [x] use `c-struct` in `GdkRectangle`
* [x] add export list
