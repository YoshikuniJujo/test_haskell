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
* [x] functions of GdkDisplay
	+ [x] gdk\_display\_open
	+ [x] gdk\_display\_get\_default
	+ [x] gdk\_display\_get\_name
	+ [x] gdk\_display\_get\_default\_screen
	+ [x] gdk\_display\_device\_is\_grabbed
	+ no gdk\_display\_beep
	+ [x] gdk\_display\_sync
	+ [x] gdk\_display\_flush
	+ [x] gdk\_display\_close
	+ [x] gdk\_display\_is\_closed
	+ [x] gdk\_display\_get\_event
	+ [x] gdk\_display\_peek\_event
	+ [x] gdk\_display\_put\_event
	+ [x] gdk\_display\_has\_pending
	+ no gdk\_display\_set\_double\_click\_time
	+ no gdk\_display\_set\_double\_click\_distance
	+ [x] gdk\_display\_supports\_cursor\_color
	+ [x] gdk\_display\_supports\_cursor\_alpha
	+ [x] gdk\_display\_get\_default\_cursor\_size
	+ [x] gdk\_display\_get\_maximal\_cursor\_size
	+ no gdk\_display\_get\_default\_group
	+ no gdk\_display\_supports\_selection\_notification
	+ no gdk\_display\_request\_selection\_notification
	+ no gdk\_display\_supports\_clipboard\_persistence
	+ no gdk\_display\_store\_clipboard
	+ no gdk\_display\_supports\_shapes
	+ no gdk\_display\_supports\_input\_shapes
	+ no gdk\_display\_get\_app\_launch\_contexts
	+ no gdk\_display\_notify\_startup\_complete
	+ [x] gdk\_display\_get\_default\_seat
	+ [x] gdk\_display\_list\_seats
	+ [x] gdk\_display\_get\_n\_monitors
	+ [x] gdk\_display\_get\_monitor
	+ [x] gdk\_display\_get\_primary\_monitor
	+ [x] gdk\_display\_get\_monitor\_at\_point
	+ [x] gdk\_display\_get\_monitor\_at\_window
* [ ] functions of GdkScreen
	+ [x] gdk\_screen\_get\_resolution
	+ [x] gdk\_screen\_list\_visuals
	+ [x] gdk\_screen\_get\_toplevel\_windows
	+ no gdk\_screen\_get\_setting
	+ [ ] gdk\_screen\_get\_font\_options
	+ [ ] gdk\_screen\_set\_font\_options
	+ [ ] gdk\_screen\_set\_resolution
	+ [x] gdk\_screen\_get\_window\_stack
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
* [ ] function of GdkVisual
	+ [x] gdk\_visual\_get\_depth
	+ [x] gdk\_visual\_get\_visual\_type
	+ [x] gdk\_visual\_get\_red\_pixel\_details
	+ [x] gdk\_visual\_get\_green\_pixel\_details
	+ [x] gdk\_visual\_get\_blue\_pixel\_details
	+ [ ] gdk\_visual\_get\_screen
	+ [ ] others
* [x] repair gdkDrawingContextGetCairoContext
	+ no GC
* [ ] repair gdkMonitorGetManufacturer
	+ process NULL pointer
* [ ] repair gdkMOnitorGetMonitor
	+ process NULL pointer
