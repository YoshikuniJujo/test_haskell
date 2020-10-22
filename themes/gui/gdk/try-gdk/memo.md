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
	+ [x] gdk\_window\_new
		- [x] correct structure of GdkWindowAttr
			* [x] define mkGdkWindowAttr
		- [x] use Ptr instead of ForeignPtr
	+ [x] gdk\_window\_destroy
	+ [x] gdk\_window\_get\_window\_type
	+ [x] gdk\_window\_get\_display
	+ [ ] gdk\_window\_get\_screen
	+ [ ] gdk\_window\_get\_visual
	+ [ ] others
* [ ] functions of GdkDisplay
	+ [x] gdk\_display\_get\_name
	+ [ ] others
