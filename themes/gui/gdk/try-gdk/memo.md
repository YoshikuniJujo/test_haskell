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
* [x] functions of GdkDisplayManager
	+ [x] gdk\_display\_manager\_get
	+ [x] gdk\_display\_manager\_get\_default\_display
	+ [x] gdk\_display\_manager\_set\_default\_display
	+ [x] gdk\_display\_manager\_list\_display
	+ [x] gdk\_display\_manager\_open\_display
* [ ] functions of GdkDisplay
	+ [x] gdk\_display\_open
	+ [x] gdk\_display\_get\_default
	+ [x] gdk\_display\_get\_name
	+ [x] gdk\_display\_get\_default\_screen
	+ [x] gdk\_display\_get\_default\_seat
	+ [ ] gdk\_display\_list\_seats
	+ [ ] gdk\_display\_device\_is\_grabbed
	+ [ ] gdk\_display\_beep
	+ [ ] gdk\_display\_sync
	+ [ ] gdk\_display\_flush
	+ [ ] gdk\_display\_close
	+ [ ] gdk\_display\_is\_closed
	+ [ ] gdk\_display\_get\_event
	+ [ ] gdk\_display\_peek\_event
	+ [ ] gdk\_display\_put\_event
	+ [ ] gdk\_display\_has\_pending
	+ [ ] gdk\_display\_set\_double\_click\_time
	+ [ ] gdk\_display\_set\_double\_click\_distance
	+ [ ] gdk\_display\_supports\_cursor\_color
	+ [ ] gdk\_display\_supports\_cursor\_alpha
	+ [ ] gdk\_display\_get\_default\_cursor\_size
	+ [ ] gdk\_display\_get\_maximal\_cursor\_size
	+ [ ] gdk\_display\_get\_default\_group
	+ [ ] gdk\_display\_supports\_selection\_notification
	+ [ ] gdk\_display\_request\_selection\_notification
	+ [ ] gdk\_display\_supports\_clipboard\_persistence
	+ [ ] gdk\_display\_store\_clipboard
	+ [ ] gdk\_display\_supports\_shapes
	+ [ ] gdk\_display\_supports\_input\_shapes
	+ [ ] gdk\_display\_get\_app\_launch\_contexts
	+ [ ] gdk\_display\_notify\_startup\_complete
	+ [ ] gdk\_display\_get\_n\_monitors
	+ [ ] gdk\_display\_get\_monitor
	+ [ ] gdk\_display\_get\_primary\_monitor
	+ [ ] gdk\_display\_get\_monitor\_at\_point
	+ [ ] gdk\_display\_get\_monitor\_at\_window
* [ ] functions of GdkSeat
	+ [x] gdk\_seat\_get\_pointer
	+ [x] gdk\_seat\_get\_keyboard
	+ [ ] others
* [ ] functions fo GdkDevice
	+ [x] gdk\_device\_get\_name
	+ [ ] others
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
	+ [x] gdk\_window\_show\_unraised
	+ [x] gdk\_window\_hide
	+ [x] gdk\_window\_is\_destroyed
	+ [x] gdk\_window\_is\_visible
	+ [x] gdk\_window\_is\_viewable
	+ [x] gdk\_window\_is\_input\_only
	+ [x] gdk\_window\_is\_shaped
	+ [x] gdk\_window\_get\_state
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
* [ ] remove or check unusable or having unknown usability functions
* [ ] functions of GdkDisplay
	+ [x] gdk\_display\_get\_name
	+ [ ] others
* [ ] functions of GdkScreen
	+ [x] gdk\_screen\_get\_resolution
	+ [ ] others
* [ ] function of GdkVisual
	+ [x] gdk\_visual\_get\_depth
	+ [ ] others
* [ ] repair gdkDrawingContextGetCairoContext
	+ no GC
