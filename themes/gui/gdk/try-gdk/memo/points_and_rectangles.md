Points and Rectangles memo
==========================

todo new
--------

* [x] make `Graphics.Gdk.PointsAndRectangles.Internal`
* [ ] refactor export list
	+ [x] GDK RECTANGLE
		- [x] `data GdkRectangle`
		- [x] `pattern GdkRectangle`
		- [x] `gdkRectangleX`
		- [x] `gdkRectangleY`
		- [x] `gdkRectangleWidth`
		- [x] `gdkRectangleHeight`
	+ [ ] GDK RECTANGLE PRIM
		- [ ] `data GdkRectanglePrim`
		- [ ] `type GdkRectangleIO`
		- [ ] `type GdkRectangleST`
		- [ ] `gdkRectangleNew`
		- [ ] `gdkRectangleFreeze`
		- [ ] `gdkRectangleThaw`
		- [ ] `gdkRectangleCopy`
	+ [ ] FUNCTION

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
