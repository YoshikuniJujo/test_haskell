memo
====

todo
----

* [x] move cairoRegionWithRectangle to simple-cairo
	+ [x] consider to use ForeignPtr to it
* [x] remove module Graphics.Gdk
* [x] repair gdkInit
	+ [x] define exception
	+ [x] free multiple CStrings
	+ use gtk\_init\_check
	+ throw exception if gdk\_init\_check return FALSE
* [x] functions of General
	+ [x] gdk\_get\_display\_arg\_name
	+ [x] gdk\_set\_allowed\_backends
	+ [x] gdk\_get\_program\_class
	+ [x] gdk\_set\_program\_class
* [ ] functions of Windows
	+ [ ] gdk\_window\_new
		- [ ] correct structure of GdkWindowAttr
			* [x] define mkGdkWindowAttr
			* [ ] others
	+ [ ] others
