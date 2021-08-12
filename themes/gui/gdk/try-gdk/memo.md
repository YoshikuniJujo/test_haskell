memo
====

todo new
--------

* [x] use `c-enum` in Values
	+ [x] `GdkWindowWindowClass`
	+ [x] `GdkAxisUse`
	+ [x] `GdkModifierType`
	+ [x] `GdkGrabStatus`
* [x] move contents of module Values to appropriate module
	+ [x] `GdkWindowWindowClass`
	+ [x] `GdkGrabStatus`
* [x] move contents of module Types to appropriate module
* [ ] refactor export list
	+ [x] Data
		- [x] Bits.Misc
		- [x] Sealed
		- [x] Sealed.Internal
	+ [x] Try.Tools
		- [x] itself
		- [x] DoWhile
	+ [x] Foreign.Ptr.Misc
	+ [ ] Graphics.Gdk
		- [x] General
			* [x] make trial
				+ [x] `gdkInit`
					- [x] try it
					- [x] check exception
				+ [x] `gdkGetDisplayArgName`
				+ [x] `gdkNotifyStartupComplete`
					- [x] remove it
				+ [x] `gdkNotifyStartupCompleteWithId`
					- [x] remove it
				+ [x] `gdkSetAllowedBackends`
				+ [x] `gdkGetProgramClass`
				+ [x] `gdkSetProgramClass`
			* [x] refactor export list
		- [ ] GdkDisplayManager
			* [x] add type `GdkDisplayManager` to export list
			* [ ] make trial
				+ [x] `gdkDisplayManagerGet`
				+ [x] `gdkDisplayManagerGetDefaultDisplay`
				+ [ ] `gdkDisplayManagerSetDefaultDisplay`
				+ [x] `gdkDsiplayManagerListDisplays`
				+ [ ] `gdkDisplayManagerOpenDisplay`
			* [x] refactor export list
			* [x] make `Graphics.Gdk.GdkDisplayManager.Internal`
		- [x] GdkDisplay
		- [x] GdkScreen
		- [x] GdkSeat
		- [x] GdkMonitor
		- [x] GdkDevice
			* [x] itself
			* [x] GdkAxes
		- [x] PointsAndRectangle
		- [x] Visuals
		- [ ] Cursors
		- [ ] Windows
			* [ ] itself
			* [ ] GdkWindowAttr
			* [ ] GdkEventMask
			* [ ] GdkModifierType
		- [ ] GdkDrawingContext
		- [ ] Events
		- [ ] EventStructures
			* [ ] GdkKeySyms
		- [ ] PropertiesAndAtoms.GdkAtom
		- [ ] Exception
* [ ] check `gdk_event_get_source_device`

module hierarchy
----------------

* Data
	+ Bits.Misc
	+ Sealed
		- Internal
* Foreign.Ptr.Misc
* Graphics.Gdk
	+ General
	+ GdkDisplayManager
		- Internal
	+ GdkDisplay
		- Internal
	+ GdkScreen
		- Internal
	+ GdkSeat
		- Internal
	+ GdkMonitor
		- Internal
	+ GdkDevice
		- Internal
		- GdkAxes
			* Internal
	+ PointsAndRectangle
	+ Visuals
	+ Cursors
	+ Windows
		- GdkWindowAttr
		- GdkEventMask
		- GdkModifierType
	+ GdkDrawingContext
	+ Events
	+ EventStructures
		- GdkKeySyms
	+ PropertiesAndAtoms.GdkAtom
	+ Exception
* Try.Tools
	+ DoWhile

API Reference contents
----------------------

### review now

* [x] General
* [x] GdkDisplayManager
* [ ] GdkDisplay
* [x] GdkScreen
* [x] GdkSeat
* [x] GdkMonitor
* [x] GdkDevice
* [x] Points and Rectangles
* [x] Visuals
* [x] Cursors
* [x] Windows
	+ [x] GdkEventMask
* [x] GdkDrawingContext
* [x] Events
* [x] EventStructures
* [x] Properties and Atoms: Atoms
	+ [x] add gdkAtomName
	+ [x] add export list

### not now

#### items

* GdkDevicePad			TINY
* Pixbufs			TINY
* RGBA Colors			TINY
* Frame clock			SMALL
* Frame timings			SMALL
* Event Structures		ONLY TYPES
* Key Values			MIDDLE
* Drag and Drop			MIDDLE
* Properties and Atoms: Properties	TINY
* Threads			SMALL
* Pango Interaction		TINY
* Cairo Interaction		SMALL
* X Window System Internaction	BIG
* Wayland Interaction		TRIBIAL
* Application launching		SMALL
* Testing			TINY

#### maybe other packages

* simple-gdk3-axes
* simple-gdk3-clipboard (selections)
* simple-gdk3-property

### all

#### big

* GdkDisplay
* GdkScreen
* GdkDevice
* Windows
* Events
* X Window System Interaction

#### middle

* General
* Visuals
* GdkGLContext
* Key Values
* Drag And Drop

#### small

* GdkSeat
* GdkMonitor
* Cursors
* Frame clock
* Frame timings
* Selections
* Properties and Atoms
* Threads
* Cairo Interaction
* Application launching

### tiny

* GdkDisplayManager
* GdkDevicePad
* Points and Rectangles
* Pixbufs
* RGBA Colors
* GdkDrawingContext
* Pango Interaction
* Testing

### only types

* Event Structures

### tribial

* Wayland Interaction

todo new
--------

* [x] use c-enum in `GdkEventType`
* [x] try `GDK_MAP`
* [x] draw when `GDK_CONFIGURE`
* [x] draw when `GDK_FOCUS_CHANGE`
* [ ] make `Graphics.Gdk.Events.GdkKeySyms`
* [ ] move `Pointerable` to `glib-stopgap`

todo old
--------

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
	+ [x] gdk\_seat\_get\_display
	+ [ ] gdk\_seat\_grab
	+ [ ] gdk\_seat\_ungrab
	+ [x] gdk\_seat\_get\_capabilities
	+ [x] gdk\_seat\_get\_pointer
	+ [x] gdk\_seat\_get\_keyboard
	+ [x] gdk\_seat\_get\_slaves
* [x] functions of GdkMonitor
	+ [x] gdk\_monitor\_get\_display
	+ [x] gdk\_monitor\_get\_geometry
	+ [x] gdk\_monitor\_get\_workarea
	+ [x] gdk\_monitor\_get\_width\_mm
	+ [x] gdk\_monitor\_get\_height\_mm
	+ [x] gdk\_monitor\_get\_manufacturer
	+ [x] gdk\_monitor\_get\_model
	+ [x] gdk\_monitor\_get\_scale\_factor
	+ [x] gdk\_monitor\_get\_refresh\_rate
	+ [x] gdk\_monitor\_get\_subpixel\_layout
	+ [x] gdk\_monitor\_is\_primary
* [ ] functions of GdkDevice
	+ [x] gdk\_device\_get\_name
	+ [x] gdk\_device\_get\_vendor\_id
	+ [x] gdk\_device\_get\_product\_id
	+ [x] gdk\_device\_get\_source
	+ [ ] gdk\_device\_set\_mode
	+ [ ] gdk\_device\_get\_mode
	+ [ ] gdk\_device\_set\_key
	+ [ ] gdk\_device\_get\_key
	+ [ ] gdk\_device\_set\_axis\_use
	+ [ ] gdk\_device\_get\_axis\_use
	+ ...
	+ [x] gdk\_device\_list\_slave\_devices
	+ ...
	+ [ ] others
* [x] functions of Points and Rectangles
	+ [x] gdk\_rectangle\_intersect
	+ [x] gdk\_rectangle\_union
	+ [x] gdk\_rectangle\_equal
* [x] function of GdkVisual
	+ [x] gdk\_visual\_get\_depth
	+ [x] gdk\_visual\_get\_visual\_type
	+ [x] gdk\_visual\_get\_red\_pixel\_details
	+ [x] gdk\_visual\_get\_green\_pixel\_details
	+ [x] gdk\_visual\_get\_blue\_pixel\_details
	+ [x] gdk\_visual\_get\_screen
* [x] functions of Cursors
	+ no gdk\_cursor\_new\_from\_pixbuf
	+ [x] gdk\_cursor\_new\_from\_surface
	+ [x] gdk\_cursor\_new\_from\_name
	+ no gdk\_cursor\_new\_for\_display
	+ [x] gdk\_cursor\_get\_display
	+ no gdk\_cursor\_get\_image
	+ [x] gdk\_cursor\_get\_surface
	+ [x] gdk\_cursor\_get\_cursor\_type
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
	+ [ ] gdk\_window\_has\_native
	+ [ ] gdk\_window\_ensure\_native
	+ [ ] gdk\_window\_reparent
	+ [ ] gdk\_window\_raise
	+ [ ] gdk\_window\_lower
	+ [ ] gdk\_window\_focus
	+ [ ] gdk\_window\_register\_dnd
	+ ...
	+ [x] gdk\_window\_begin\_draw\_frame
	+ [x] gdk\_window\_end\_draw\_frame
	+ ...
	+ [x] gdk\_window\_set\_title
	+ [x] gdk\_window\_set\_cursor
		- [ ] repair to set NULL
	+ [x] gdk\_window\_get\_cursor
		- [x] define GdkCursorRef
		- [ ] repair to get NULL: convert to Nothing
	+ [ ] gdk\_window\_get\_geometry
	+ ...
	+ [x] gdk\_window\_get\_width
	+ [x] gdk\_window\_get\_height
	+ ...
	+ [x] gdk\_window\_get\_position
	+ [ ] gdk\_window\_get\_root\_origin
	+ [ ] gdk\_window\_get\_frame\_extents
	+ [ ] gdk\_window\_get\_origin
	+ [ ] gdk\_window\_get\_root\_coords
	+ [ ] gdk\_window\_get\_device\_position
	+ [ ] gdk\_window\_get\_device\_position\_double
	+ [x] gdk\_window\_get\_parent
	+ [ ] gdk\_window\_get\_toplevel
	+ [ ] gdk\_window\_get\_children
	+ ...
	+ [x] gdk\_window\_get\_events
	+ [ ] gdk\_window\_set\_events
	+ ...
	+ [ ] gdk\_window\_set\_decorations
	+ [x] gdk\_window\_get\_decorations
	+ ...
	+ [x] gdk\_get\_default\_root\_window
	+ ...
	+ [ ] gdk\_window\_get\_device\_cursor
	+ [x] gdk\_window\_set\_device\_cursor
	+ [ ] gdk\_window\_get\_device\_events
	+ [x] gdk\_window\_set\_device\_events
	+ [ ] gdk\_window\_get\_source\_events
	+ [x] gdk\_window\_set\_source\_events
	+ ...
	+ [ ] gdk\_window\_coords\_from\_parent
	+ [ ] gdk\_window\_coords\_to\_parent
	+ ...
	+ [ ] others
* [x] functions of GdkDrawingContext
	+ [x] gdk\_draiwng\_context\_get\_window
	+ [x] gdk\_drawing\_context\_get\_clip
	+ [x] gdk\_drawing\_context\_get\_cairo\_context
	+ [x] gdk\_drawing\_context\_is\_valid
* [ ] functions of Events
	+ [x] gdk\_events\_pending
	+ [x] gdk\_event\_peek
	+ [x] gdk\_event\_get
	+ [x] gdk\_event\_put
	+ [x] gdk\_event\_new
	+ [x] gdk\_event\_copy
	+ [x] gdk\_event\_free
	+ [x] gdk\_event\_get\_axis
	+ [x] gdk\_event\_get\_button
	+ [x] gdk\_event\_get\_click\_count
	+ [x] gdk\_event\_get\_coords
	+ [x] gdk\_event\_get\_keycode
	+ [x] gdk\_event\_get\_keyval
	+ [x] gdk\_event\_get\_root\_coords
	+ [x] gdk\_event\_get\_scroll\_direction
	+ [x] gdk\_event\_get\_scroll\_deltas
	+ [x] gdk\_event\_get\_scroll\_stop\_event
	+ [x] gdk\_event\_get\_state
	+ [x] gdk\_event\_get\_time
	+ [x] gdk\_event\_get\_window
	+ [x] gdk\_event\_get\_event\_type
		- [x] repair gdk\_event\_get_event\_type
	+ [ ] gdk\_event\_get\_event\_sequence
	+ no gdk\_event\_request\_motions
	+ [ ] gdk\_event\_get\_angle
	+ [ ] gdk\_event\_get\_center
	+ [ ] gdk\_event\_get\_distance
	+ [ ] gdk\_event\_triggers\_context\_menu
	+ [x] gdk\_event\_get\_seat
	+ [x] gdk\_event\_get\_scancode
	+ [ ] gdk\_event\_get\_pointer\_emulated
	+ [ ] gdk\_event\_handler\_set
	+ [x] gdk\_get\_show\_events
	+ [ ] gdk\_set\_show\_events
	+ [x] gdk\_event\_get\_screen
	+ [x] gdk\_event\_set\_screen
	+ [x] gdk\_event\_get\_device
	+ [x] gdk\_event\_set\_device
	+ [x] gdk\_event\_get\_source\_device
	+ [x] gdk\_event\_set\_source\_device
	+ [x] gdk\_event\_get\_device\_tool
	+ [ ] gdk\_event\_set\_device\_tool
	+ [ ] gdk\_setting\_get
	+ [ ] others
* [ ] repair gdkEventGetSourceDevice
	+ [x] check problem
	+ [x] change GdkDevice: use ForeignPtr instead of Ptr
	+ [x] gdkEventGetSourceDevice: add touchForeignPtr to finalizer of GdkDevice
	+ [x] check problem
	+ [ ] git pull; stack build
	+ [ ] check no problem
* [ ] remove or check unusable or having unknown usability functions
* [x] repair gdkDrawingContextGetCairoContext
	+ no GC
* [ ] repair gdkMonitorGetManufacturer
	+ process NULL pointer
* [ ] repair gdkMOnitorGetMonitor
	+ process NULL pointer
* [ ] unify Foo and FooRef
	+ `data Foo = Foo (ForeignPtr Foo) | FooRef (Ptr Foo) deriving Show`
* [ ] gdk\_event\_get\_source\_device
