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
* [ ] functions of General
	+ [x] gdk\_get\_display\_arg\_name
	+ [x] gdk\_set\_allowed\_backends
	+ [ ] gdk\_get\_program\_class
	+ [ ] gdk\_set\_program\_class
