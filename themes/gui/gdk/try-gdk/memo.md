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
	+ [x] gdk\_window\_get\_screen
	+ [x] gdk\_window\_get\_visual
	+ [x] gdk\_window\_show
	+ [ ] gdk\_window\_show\_unraised
	+ [ ] gdk\_window\_hide
	+ [ ] gdk\_window\_is\_destroyed
	+ [ ] gdk\_window\_is\_visible
	+ [ ] gdk\_window\_is\_viewable
	+ [ ] gdk\_window\_is\_input\_only
	+ [ ] gdk\_window\_is\_shaped
	+ [ ] gdk\_window\_get\_state
	+ [ ] gdk\_window\_withdraw
	+ [ ] gdk\_window\_iconify
	+ [ ] gdk\_window\_deiconify
	+ [ ] gdk\_widnow\_stick
	+ [ ] gdk\_window\_unstick
	+ [ ] gdk\_window\_maximize
	+ [ ] gdk\_window\_unmaximize
	+ [ ] gdk\_window\_fullscreen
	+ [ ] gdk\_window\_fullscreen\_on\_monitor
	+ [ ] gdk\_window\_unfullscreen
	+ [ ] gdk\_window\_get\_fullscreen\_mode
	+ [ ] gdk\_window\_set\_fullscreen\_mode
	+ [ ] gdk\_window\_set\_keep\_above
	+ [ ] gdk\_window\_set\_keep\_below
	+ [ ] gdk\_window\_set\_opacity
	+ [ ] gdk\_window\_set\_composited
	+ [ ] gdk\_window\_get\_composited
	+ [ ] gdk\_window\_set\_pass\_through
	+ [ ] gdk\_window\_get\_pass\_through
	+ [ ] gdk\_window\_move
	+ [ ] gdk\_window\_resize
	+ [ ] gdk\_window\_move\_resize
	+ [ ] gdk\_window\_scroll
	+ [ ] gdk\_widnow\_move\_to\_rect
	+ [ ] gdk\_widnow\_move\_region
	+ [ ] others
* [ ] functions of GdkDisplay
	+ [x] gdk\_display\_get\_name
	+ [ ] others
* [ ] functions of GdkScreen
	+ [x] gdk\_screen\_get\_resolution
	+ [ ] others
* [ ] function of GdkVisual
	+ [x] gdk\_visual\_get\_depth
	+ [ ] others
