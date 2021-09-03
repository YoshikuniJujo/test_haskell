memo
====

todo
----

* [x] add `Map GdkWindow WindowId`
* [x] handler to process window new event and mouse down event
* [ ] others

structure
---------

* Moffy (10)
	+ Control.Moffy
		- NoThreadId
		- Handle
		- Run
		- Internal
			* Sig
			* Sig.Type
			* React
			* React.Type
		- Event.ThreadId
		- Handle.ThreadId
* Moffy library (20)
	+ Control.Moffy.Event
		- Lock
			* Internal
		- Random
			* Internal
		- Time
		- Delete
		- Key
			* Internal
			* Internal.XK
		- Mouse
		- CalcTextExtents
	+ Control.Moffy.Handle
		- Lock
		- Random
		- Time
		- GtkField
		- XField
			* Key
			* Mouse
			* CalcTextExtents
* Moffy base (9)
	+ Data.OneOrMore (1)
	+ Data.Type.Set (2)
		+ Internal
	+ Control.Monad.Freer.Par (6)
		- Sequence
		- Funable
		- FTCQueue
		- TaggableFunction
		- Internal.Id
* GTK (7)
	+ Graphics.Gtk
		- Cairo
			* Values
		- CairoType
		- Pango
		- Values
		- AsPointer
* Trial (23)
	+ TryRandom
	+ Count
	+ TryKey
	+ TryLock
	+ TryThreadId
	+ TrySharing
		- ThreadId
	+ TryCalcTextExtents
	+ Boxes (5)
		- Box
		- BoxEv
		- Run
		- View
	+ Followbox (10)
		- Clickable
		- Event
		- Handle
		- XFieldHandle
		- View
		- ViewType
		- Run
		- RunGtk
		- TypeSynonym

todo
----

* [x] repair Control.Moffy.Internal.React.first
	+ [x] rename AdjustableAdjustable
	+ [x] remove first
	+ [x] rename first' to first
* [x] refactor Control.Moffy.Internal.React
* [x] refactor Control.Moffy.Internal.Sig
* [x] refactor Control.Moffy.Internal.React.Type
	+ [x] move constraint synonym FooOccurred to here
* [x] refactor Control.Moffy.Handle
* [x] separate WithThreadId and WithNoThreadId
	+ [x] add NoThreadId and noThreadId
	+ [x] correct module Control.Moffy.Internal.React and Control.Moffy.Internal.Sig
		- [x] define functions
			* first_, at_, break_, until_, indexBy_, parList_
			* `foo_ :: React s es (ThreadId, ThreadId) -> ...`
	+ [x] correct module Control.Moffy
		- [x] move definition of first, at, break, until, indexBy, parList from Internal.React and Internal.Sig
			* [x] first
			* [x] at
			* [x] break
			* [x] until
			* [x] indexBy
			* [x] parList
	+ [x] make module Control.Moffy.NoThreadId
		- [x] define functions
			* [x] app'
			* [x] iapp'
			* [x] first'
			* [x] at'
			* [x] break' and until'
			* [x] indexBy'
			* [x] parList'
* [x] try with Trial.TrySharing
* [x] refactor Control.Moffy
* [x] refactor Control.Moffy.NoThreadId
* [x] refactor Control.Moffy.Internal.Sig
* [x] refactor Control.Moffy.Internal.React
* [x] refactor Control.Moffy.Internal.React.Type
* [x] refactor Control.Moffy.Handle
* [x] make handle using GTK
	+ [x] consider to use TChan in Handle.TChan
	+ [x] handle Delete Event
	+ [x] handle Mouse Event
		- [x] MotionNotifyEvent
			* [x] make MotionNotifyEvent
			* [x] add gtk_widget_set_events
		- [x] ButtonPressEvent and ButtoReleaseEvent
			* [x] extract button
			* [x] extract x and y
			* [x] ButtonPressEvent
			* [x] ButtonReleaseEvent
	+ [x] handle Key Event
		- [x] KeyPressEvent and KeyReleaseEvent
			* [x] KeyPressEvent
			* [x] KeyReleaseEvent
	+ [x] remove Control.Moffy.Handle.TimeTChan
	+ [x] draw rectangle
		- [x] draw contour
		- [x] set color
		- [x] fill rectangle
	+ [x] free Mutable
	+ [x] test followbox
		- [x] draw character
		- [x] draw image
			* [x] try to use cairo_image_surface_create_from_png from C
			* [x] try to use cairo_image_surface_create_from_png from Haskell
			* [x] try to use cairo_image_surface_create_from_png_stream from C
			* [x] try to use cairo_image_surface_create_from_png_stream from Haskell
		- [x] draw image from web
		- [x] resize image
		- [x] investigate about XGlyphInfo
			* cairo_text_extents_t
			* cairo_text_extents
		- [x] consider whether or not to change Point from using Integer to using Double
		- [x] consider whether or not to change textExtents from using Integer to using Double
		- [x] rename module Trial.Followbox.Xrender to Trial.Followbox.TextExtents
		- [x] define TextExtents
			* `data TextExtents { `
			* `textExtentsXBearing :: Double, textExtentsYBearing :: Double,`
			* `textExtentsWidth :: Double, textExtentsHeight :: Double,`
			* `textExtentsXAdvance :: Double, textExtentsYAdvance :: Double }`
		- [x] move TextExtents event to Control.Moffy.Event.TextExtents
			* [x] move CalcTextExtents
			* [x] move TextExtents to module Control.Moffy.Event.CalcTextExtents
			* [x] move TypeSynonyms to module Control.Moffy.Event.CalcTextExtents
		- [x] move TextExtents handle to Control.Moffy.Handle.XField.TextExtents
		- [x] add TextExtents handle to Control.Moffy.Handle.XField.handle
		- [x] add trial for TextExtents
		- [x] try pango_layout_get_extents and pango_layout_get_pixel_extents
		- [x] change TextExtents
			* [x] define TextExtents'
			* [x] define calcTextExtents'
			* [x] make wrapper of pango_layout_get_extents
				+ [x] pango_cairo_create_layout
				+ [x] pango_layout_set_text
				+ [x] pango_font_description_from_string
				+ [x] pango_font_description_set_size
				+ [x] pango_font_description_set_absolute_size
				+ [x] pango_layout_set_font_description
				+ [x] difference of size and absolute size
				+ [x] pango_layout_get_extents
				+ [x] pango_layout_get_pixel_extents
				+ [x] pango_cairo_show_layout
			* [x] show pango extents in GTK main loop
			* [x] move string
			* [x] use calcTextExtents'
			* [x] change signal
				+ [x] move converter from Event to Handle
		- [x] define TextExtents Handler on GTK
		- [x] add trial for TextExtents on GTK
		- [x] make handleFollowboxWith
		- [x] make handler for followbox on GTK
			* [x] make Trial.Followbox.RunGtk
		- [x] try followbox with print
		- [x] move png converter to viewer
			* [x] separate getAvatar to getAvatarPng and decodeAvatar
			* [x] rename decodeAvatar to decodePng
			* [x] copy decodePng to viewer
			* [x] change View type to use Png
			* [x] repair getAvatar and viewer
			* [x] remove decodePng at module Trial.Followbox
		- [x] define instance Drawable [view type of Followbox]
			* [x] show text
			* [x] set font and size
			* [x] set color
			* [x] show line
			* [x] show image
				+ [x] show png
				+ [x] show jpg
					- [x] read multiple format image from ByteString
					- [x] write PNG format to ByteString
					- [x] use these
		- [x] try followbox
	+ [x] refactoring
		- [x] remove module Arr
		- [x] view module hierarchy
* [x] make newtype SetApp
	+ [x] make module Data.Type.SetApp.Internal
	+ [x] define `newtype SetApp a = SetApp (Type -> Type) (Set a)`
* [x] make module Data.OneOrMore.Internal
* [x] make newtype OneOrMoreApp
	+ [x] make module Data.OneOrMoreApp
	+ [x] define OneOrMoreApp
	+ [x] make pattern Singleton
	+ [x] class and instance ExpandableApp, CollapsableApp
* [x] use OneOrMoreApp in EvOccs
* [x] remove old applied Set
* [ ] rename new applied Set
	+ [x] rename `(:$:.)` to `(:$:)`
	+ [x] Data.OneOrMoreApp: refactor API some
	+ [ ] consider whether or not to remove suffix `App'
		- or change to Suffix \'
	+ [ ] others
* [x] make OneOrMore function strict
	+ [x] `(>-)`
	+ [x] merge
	+ [x] merge'
* [x] make OneOfThem
	+ [x] add Data.OneOfThem
	+ [x] Singleton
	+ [x] expand
	+ [x] collapse
	+ [x] `class InsertFun f (OneOfThemFun fs) where (>--) ...`
	+ [x] `(>--.) :: (a -> b) -> OneOfThemFun as b -> OneOfThemFun (a :- as) b`
	+ [x] `(>-) :: OneOfThem as -> [OneOfThem as'] -> [OneOfThem (as :+: as')]`
	+ [x] rename `(>--.)` to `(>--$$$)`
	+ [x] rename `(>--)` to `(>--.)`
	+ [x] rename `(>--$$$)` to `(>--)`
	+ [x] infixr 5 `(>-)`
	+ [x] infixr 5 `(>--)`
	+ [x] make export list of Data.OneOfThem
	+ [x] define mergeFun : `mergeFun :: OneOfThemFun as -> OneOfThemFun as' -> OneOfThemFun (as :+: as')`
* [x] separate View1 of followbox
	+ [x] define drawTextGtk, drawLineGtk and drawImageGtk
	+ [x] change View1 definition: `View1 :: OneOfThem (Text :- Line :- Image :- 'Nil)`
	+ [x] repair others
* [x] make view modules
	+ [x] separate Text, Line and Image to each module
		- [x] module Trial.Followbox.Basic: Color and Position
		- [x] module Trial.Followbox.Text
		- [x] module Trial.Followbox.Shape
		- [x] module Trial.Followbox.Image
		- [x] module Trial.Followbox.XField
		- [x] module Trial.Followbox.GtkField
		- [x] module Trial.Followbox.ViewType
	+ [x] make module directory: Control.Moffy.Viewable
		- [x] module Control.Moffy.Viewable.Basic
		- [x] module Control.Moffy.Viewable.Text
		- [x] module Control.Moffy.Viewable.Shape
		- [x] module Control.Moffy.Viewable.Image
	+ [x] make module directory: Control.Moffy.View
		- [x] module Control.Moffy.View.XField
		- [x] module Control.Moffy.View.GtkField
	+ [x] move each modules under Control.Moffy.View
* [x] use OneOfThem in view of GTK
	+ [x] define tryUseTChanGen: `tryUseTChanGen :: (GtkWidget -> CairoT -> a -> IO ()) -> IO (...)`
	+ [x] remove instance Draw View
		- [x] define drawFollowboxGtk
		- [x] use tryUseTChanGen
	+ [x] move Box to Control.Moffy.Viewable
	+ [x] define drawBox
	+ [x] use tryUseTChanGen about Box
	+ [x] remove class Drawable
	+ [x] move contents of Trial.Boxes.XField to Control.Moffy.View.XField
	+ [x] remove Trial.Boxes.XField
	+ [x] move contents of Trial.Boxes.GtkField to Control.Moffy.View.GtkField
	+ [x] remove Trial.Boxes.GtkField
* [x] remove Trial.Followbox.XField
	+ [x] rename Trial.Followbox.Run to Trial.Followbox.RunXField
	+ [x] others
* [x] remove Trial.Followbox.GtkField
	+ [x] rename Trial.Followbox.RunGtk to Trial.Followbox.RunGtkField
	+ [x] others
* [x] separate Control.Moffy.Handle.GtkField
	+ [x] refactor Control.Moffy.Handle.GtkField some
	+ [x] move tryUseTChanGen to Control.Moffy.Run.GtkField
	+ [x] more general Control.Moffy.Handle.GtkField.handle
	+ [x] rename Control.Moffy.Handle.GtkField to Control.Moffy.Handle.TChan
* [x] remove suffix App
* [x] refactor Control.Moffy.Run.GtkField some
	+ [x] rename tryUseTChanGen to runGtkMain
	+ [x] refactor some
	+ [x] refactor more
		- [x] refactor function lastTChan
		- [x] refactor function draw
		- [x] refactor function runGtkMain
			* [x] define handler function of gdk event
			* [x] separate the function into 4 modules
				+ [x] make window and signal connect
				+ [x] make draw area and signal connect
				+ [x] run process to recieve viewable
				+ [x] run process to recieve request to calculate text extents
					- [x] define it
					- [x] refector it
* [x] rename Trial.Boxes.Run to Trial.Boxes.RunXField
* [x] make Trial.Boxes.RunGtkField
* [x] make Control.Moffy.Run.TChan
	+ define interpret, interpretSt
	+ `interpret :: (...) => Handle m es' -> TChan a -> Sig s es a r -> m r`
	+ `interpretSt :: (...) => HandleSt st m es' -> TChan a -> Sig s es a r -> St st m r`
	+ [x] define interpretSt' in Boxes
	+ [x] move interpretSt' to Control.Moffy.Run.TChan
	+ [x] rename interpretSt' to interpretSt
	+ [x] define interpret
* [x] use Control.Moffy.Run.TChan
	+ [x] Followbox
* [x] bug fix: exposeBoth
* [x] define withLockSig
* [x] scroll event
	+ [x] make Trial.TryScroll
	+ [x] make app/tryScroll.hs
	+ [x] add scroll event to gSignalConnect
	+ [x] add getter function of GdkEventScroll
	+ [x] make scroll event in Control.Moffy.Event.Mouse
	+ [x] use scroll event in Trial.TryScroll
* [ ] make draw application
	+ [x] make Trial.Draw
	+ [x] make app/draw.hs
	+ [x] sample one line
	+ [x] no fill lines
	+ [ ] roughly-click function
		- [x] draw box
		- [x] get click inside the rectangle
		- [x] draw box on line start and end points
		- **$Oops!!!$**
		- [x] remove StateT etc
		- [x] layer: line above box
			* [x] define sortType
			* [x] try to use sortType
			* [x] use sortType
		- [x] make StoreLines and LoadLines
		- [x] add viewable message
		- [x] make react to add line
			* [x] define addLine
			* [x] define handle
			* [x] add LineEv to draw
				- [x] use addLine
				- [x] test with message
		- [x] roughly-button-up function
		- [x] more large point square
		- [ ] remove lock from addLine
		- [x] draw rectangle
	+ [x] fill
		- [x] select point by right click
			* [x] change color of point square
		- [x] add FillPolygon
			* [x] prepare tryFillPolygon
			* [x] add FillPolygon to Control.Moffy.Viewable.Shape
			* [x] add FillPolygon to Control.Moffy.View.GtkField
		- [x] draw with gray
			* [x] add FillPolygon to types
			* [x] emit polygon
		- [x] make rgb area
			* [x] show box of red, green and blue
			* [x] each color plus or minus with wheele
		- [x] set fill shape and color with left click
		- [x] more shape and color
	+ [x] save
		- [x] make save format
			* [x] line and fill polygon
		- [x] load as background
	+ [ ] make rectangle mode
	+ [ ] no fill rectangle
	+ [ ] no fill lines and polygon
	+ [ ] Bezier curve
* [x] repair handle of Control.Moffy.Handle.XField
* [ ] about window
	+ [x] create window function
	+ [x] add window identity
	+ [x] use windowNew
	+ [x] make TVar WindowId
	+ [x] use different window id to each window
	+ [x] change vda: Maybe GtkWidget ==> Map WindowId GtkWidget
		- [x] use (WindowId 0) every time
		- [x] use different window id to each drawing area
	* [x] multi window calc text extents
		- [x] add window id to calc text extents event
		- [x] use window id to calc text extents
	+ [x] make multiple window trial
		- [x] try drawing box
	+ [x] destroy window with delete event
		- [x] delete event has window id
		- [x] add gtkWidgetDestroy to Graphics.Gtk
		- [x] add TVar (Map WindowId GtkWidget) to save windows
		- [x] make destroy window request
	+ [x] pass WindowId to deleteEvent
	+ [x] viewable has window id
	+ [x] add window id to mouse event
		- [x] mouseDown
		- [x] mouseUp
		- [x] mouseMove
		- [x] mouseScroll
	+ [x] add window id to key event
		- [x] keyDown
		- [x] keyUp
	+ [x] add window id to delete event
	+ [x] add window id to calcTextExtents
	+ [x] consider about default WindowId
		- [x] make Control.Moffy.Event.DefaultWindow
		- [x] make Control.Moffy.Handle.DefaultWindow
		- [x] make trial to try DefaultWindow (store and load)
		- [x] make Control.Moffy.Event.Mouse.DeafultWindow
			* [x] mouseDown and so on
				+ [x] Trial.Count
				+ [x] Trial.TryLock
				+ [x] Trial.TrySharing
				+ [x] Trial.TryThreadId
				+ [x] Trial.TrySharing.ThreadId
				+ [x] Trial.Boxes
				+ [x] Trial.Draw
				+ [x] Trial.Followbox.Clickable
			* [x] mouseUp and so on
			* [x] mouseMove and so on
			* [x] mouseScroll and so on
		- [x] make Control.Moffy.Event.Key.DeafultWindow
			* [x] make empty module
			* [x] keyDown
			* [x] keyUp
		- [x] make Control.Moffy.Event.Delete.DefaultWindow
		- [x] make Control.Moffy.Event.CalcTextExtents.DefaultWindow
		- [ ] make Control.Moffy.Event.Window.DefaultWindow
		- [ ] make Control.Moffy.Event.Cursor.DefaultWindow
	+ [x] get window size or drawing area size
		- [x] make ConfigureEvent in Graphics.Gtk
		- [x] make ConfigureEvent in Control.Moffy.Event.Window
		- [x] make handler in Control.Moffy.Run.GtkField
		- [x] make trial Trial.TryConfigureEvent
* [x] repair text extents
* [x] about cursor
	+ [x] define gtkWidgetGetWindow
	+ [x] define gdkWindowGetDisplay
	+ [x] define gdkCursorNewFromName
	+ [x] define gdkWindowSetCursor
	+ [x] try use above functions
	+ [x] define named cursor in Control.Moffy.Event.Cursor
		- [x] consider what should do when return value is NULL
	+ [x] change cursor to prepared cursor from name
		- [x] add handler to Control.Moffy.Run.GtkField
	+ [x] define gdkCursorNewFromSurface
	+ [x] try to change cursor from PNG
	+ [x] define PNG cursor in Control.Moffy.Viewable.Cursor
	+ [x] change cursor picture from PNG
		- [x] add handler to Control.Moffy.Run.GtkField
		- [x] get sample PNG
		- [x] add to Trial.TryCursor
* [ ] make Graphics.Gdk
	+ [ ] make library
		- [x] gdkWindowNew and so on
		- [x] free GdkEvent
		- [ ] match GdkEventFoo
			* [x] print Event type
			* [x] GdkEvent GdkEventType (Ptr GdkEvent)
			* [x] separate Graphics.Gdk.Event
			* [ ] GdkEvent GdkEventType (Ptr GdkEvent) ==> GdkEventFoo GdkEventFoo
				+ [x] GdkEventConfigure
				+ [x] GDK_WINDOW_STATE
				+ [x] GDK_MAP
				+ [x] GDK_VISIBILITY_NOTIFY
				+ [x] GDK_DELETE
				+ [x] GDK_NOTHING
				+ [x] GDK_KEY_PRESS
				+ [x] GDK_KEY_RELEASE
				+ [x] GDK_UNMAP
				+ [x] GDK_FOCUS_CHANGE
				+ [ ] others
			* [x] use ForeignPtr to GdkEvent and so on
		- [ ] others
	+ [ ] FooMask
		+ [x] ExposureMask
			- GDK_EXPOSE_EVENT may be legacy
		+ [ ] FooMask about mouse button click
			- [x] GDK_BUTTON_PRESS_MASK
			- [ ] others
		+ [ ] FooMask about mouse motion
	+ [ ] sample tryGdk
		- [x] stroke line
		- [x] no forever
		- [x] try using pattern synonyms
		- [ ] draw if Visibility event occur
			+ [x] get Window
			+ [x] always
			+ [ ] only visible
		- [ ] others
	+ [ ] use ForeignPtr some object of Graphics.Gdk
	+ [ ] use Either FooValue FooForeignPtr as argument to pass event
* [ ] make Control.Moffy.Handle.GdkField
* [ ] make Control.Moffy.View.GdkField
* [ ] refactor Graphics.Gtk and Graphics.Gtk.\*
* [ ] separate Graphics.Gtk and Graphics.Gtk.\* to independent package
* [ ] move DeleteEvent from Control.Moffy.Event.Delete to Control.Moffy.Event.Window
* [x] pass argument to `runGtkMain`
* [ ] consider to move `gtkMainQuit` to GTK thread
* [ ] consider to use `Sig s 'Nil a r` and `React s 'Nil r`
* [ ] move deleteEvent to Control.Moffy.Event.Window
* [ ] consider whether or not to weeken cohesion
* [ ] remake key event
	+ [x] consider Pattern vs Value
	+ use #enum of hsc2hs
	+ [x] copy /usr/include/X11/keysymdef.h to /include/X11/
	+ [x] add include-dirs to package.yaml
	+ [x] make template to define Pattern from value
	+ [ ] consider to move key patterns to other modules and other packages
	+ [ ] make Control.Moffy.Event.Key.Internal.TryKeyValue
		- [x] XK_VoidSymbol
		- [x] TTY function keys
		- [ ] International & multi-key character composition
		- [ ] Japanese keyboard support
		- [ ] ? XK_KOREAN
		- [ ] Cursor control & motion
		- [ ] Misc functions
		- [ ] Keypad functions, keypad numbers cleverly chosen to map to ASCII
		- [ ] Auxiliary functions
		- [ ] Modifiers
		- [ ] Keyboard (XKB) Extension function and modifier keys
		- [ ] extra dead elements for German T3 layout
		- [ ] dead vowels for universal syllable entry
		- [ ] Single-Stroke Multiple-Character N-Graph Keysyms for The X Input Method
		- [ ] 3270 Terminal Keys Byte 3 = 0xfd
		- [ ] Latin 1 (ISO/IEC 8859-1 = Unicode U+0020..U+00FF) Byte 3 = 0
		- [ ] Latin 2 Byte 3 = 1
		- [ ] Latin 3 Byte 3 = 2
		- [ ] Latin 4 Byte 3 = 3
		- [ ] Latin 8
		- [ ] Latin 9 Byte 3 = 0x13
		- [ ] Katakana Byte 3 = 4
		- [ ] Arabic Byte 3 = 5
		- [ ] Cyrillic Byte 3 = 6
		- [ ] Greek (based on an early draft of, and not quite identical to, ISO/IEC 8859-7 Byte 3 = 7
		- [ ] Technical (from the DEC ...) Byte 3 = 8
		- [ ] Speciual (from the DEC VT100 ...) Byte 3 = 9
		- [ ] Publishing Byte 3 = 0x0a
		- [ ] APL Byte 3 = 0x0b
		- [ ] Hebrew Byte 3 = 0x0c
		- [ ] Thai Byte 3 = 0x0d
		- [ ] Korean Byte 3 = 0x0e
		- [ ] Hungul Consonant Characters
		- [ ] Hungul Vowel Characters
		- [ ] Hangul syllable-final (JongSong) Characters
		- [ ] Ancient Hangul Consonant Characters
		- [ ] Ancient Hangul Vowel Characters
		- [ ] Ancient Hangul syllable-final (JongSong) Characters
		- [ ] Korean currency symbol
		- [ ] Armenian
		- [ ] Georgian
		- [ ] Azeri
		- [ ] Vietnamese
		- [ ] Sinhala
	+ [x] use new module
	+ [x] remove old module
	+ [ ] rename and arrange module hierarchy
	+ [ ] others
* [ ] consider whether or not to change return type of mousePos from () to a
* [ ] consider whether or not to add Control.Moffy.Event.Mouse.DefaultWindow.MouseEv
* [ ] refactoring
	+ [ ] refactor Moffy (10)		<- now
	+ [ ] refactor Moffy library (20)
	+ [ ] refactor Moffy base (9)
	+ [ ] refactor GTK (7)
	+ [ ] refactor Trial (23)
	+ [ ] refactor app/foo.hs (8)
* [ ] check module hierarchy
	+ [ ] Moffy
	+ [x] Moffy library
	+ [ ] Moffy base
	+ [ ] Trial
* [ ] refactoring
	+ [x] Control.Moffy
	+ [ ] module structure of Control.Moffy.Event.Key
	+ [ ] Control.Moffy.Event.Key
	+ [ ] Trial
	+ [ ] Control.Monad.Freer.Par
	+ [ ] Data.OneOrMore
	+ [ ] Control.Moffy
		- [ ] consider rename adjust and adjustSig
			* [ ] adjust -> adjustReact ?
			* [ ] adjustSig -> adjust ?
		- [ ] others
	+ [ ] Data.Type.Set
		- [ ] numbered use fixed 64
		- [ ] others
	+ [x] Moffy
	+ [x] Moffy library
	+ [ ] Moffy base
* [ ] move time event to Moffy library
* [ ] separate
	+ [ ] Trial
	+ [ ] Data.OneOrMore (with Data.Type.Set)
	+ [ ] Data.Type.Set or not
	+ [ ] Control.Monad.Freer
	+ [ ] Moffy library
		- [ ] Lock
		- [ ] Random
		- [ ] Delete, Key and Mouse
		- [ ] XField
* [ ] make tear-off sample
* [ ] make tetris like game
	+ [ ] moffyris
* [ ] consider whether to remove interpret and interpretReact
	+ [ ] remove interpret and interpretReact
	+ [ ] rename interpretSt and interpretReactSt
		to interpret and interpretReact
* [ ] consider whether to remove Handle and Handle'
	+ [ ] remove Handle and Handle'
	+ [ ] rename HandleSt and HandleSt' to Handle and Handle'
	+ [ ] consider whether to add (for example) function simple
		to make SimpleHandle
* [ ] consider whether or not to put `deriving Show' to Occurred Foo
* [ ] define Data.Or.or
* [ ] use DrawingArea size instead of Window size

Graphics.Gtk (7)
----------------

### modules

```
Graphics
  +- Graphics.Gtk
  |    +- Graphics.Gtk.Values
  |    +- Graphics.Gtk.AsPointer
  +- Graphics.Cairo
  |    +- Graphics.Gtk.Cairo.Values
  +- Graphics.CairoType
  +- Graphics.Pango
```

### module hierarchy

```
Graphics.Gtk
  +- Graphics.Gtk.Value
  +- Graphics.Gtk.AsPointer
  +- Graphics.CairoType

Graphics.Cairo
  +- Graphics.Cairo.Value
  +- Graphics.CairoType
  +- Graphics.Gtk.AsPointer

Graphics.Pango
  +- Graphics.CairoType
```

### todo

* [x] make type hierarchy of GObject
	+ [x] make module System.Gobject.ObjectHierarchy
	+ [x] define data SomeGObject and class GObject
	+ [x] define `instance GObject SomeGObject`
	+ [x] define gobjectHierarchy
	+ [x] try gobjectHierarchy
	+ [x] change form `pointer :: a -> Ptr a` to `pointer :: a -> (Ptr a -> IO b) -> IO b`
	+ [x] make empty module System.Gobject.SignalConnect
	+ [x] define `gSignalConnect' :: (GObject o => o -> ...`
		- [x] define class Signal
			* `class Signal s where type Receiver s`
	+ [x] try delete event
		- [x] define GInitialUnowned
		- [x] define GtkWidget
		- [x] define GtkContainer
		- [x] define GtkBin
		- [x] define GtkWindow
		- [x] define windowNew for test
		- [x] define gtkWidgetShowAll for test
		- [x] try to show window
		- [x] define delete event for test
		- [x] try new gSignalConnect
	+ [x] define gCastObject
	+ [x] import Control.Exception.Hierarchy
	+ [x] define exception for cast error
		- `FooException TypeRep TypeRep`
	+ [x] define gCastObjectIo
		- [x] use exceptionHierarchy
	+ [x] add gCastObject to gSignalConnect to check type
	+ [x] move Reciever s into Callback
	+ [x] repair first argument of Callback
		- [x] add type argument to Callback and CCallback
		- [x] add modifyPointer to class Pointer
		- [x] repair DeleteEvent
		- [x] repair DrawEvent
		- [x] remove value from class Pointer
* [ ] use gSignalConnect\' in GtkWindow
	+ [x] make DrawingArea
	+ [x] make Draw event
	+ [x] define gtkDrawingAreaNew
	+ [x] define gtkContainerAdd
	+ [x] make test app to print Wigdet and CairoT
	+ [x] use gCastObjectIo instead of fromJust . gCastObject
	+ [x] try to use new drawing area
		- [x] use new DrawingAreaNew
		- [x] use new gSignalConnect to DrawingArea
	+ [x] try to use new window
		- [x] use new gtkWindowNew
		- [x] use new gSignalConnect to DeleteEvent
	+ [ ] make signals
		- [x] make System.Gobject.TempEvents
		- [ ] KeyPressEvent and KeyReleaseEvent
		- [ ] ButtonPressEvent and ButtonReleaseEvent
		- [ ] ScrollEvent
		- [ ] MOtionNotifyEvent
		- [ ] ConfigureEvent
	+ [ ] use above new events
		- [ ] KeyPressEvent and KeyReleaseEvent
			* [x] make tryKey
			* [ ] use above to above
		- [ ] ButtonPressEvent and ButtonReleaseEvent
		- [ ] ScrollEvent
		- [ ] MOtionNotifyEvent
		- [ ] ConfigureEvent
	+ [ ] remove old GtkWindow and old GtkDrawingArea
	+ [ ] make test app for DrawingArea and DrawEvent
	+ [ ] use new DrawingArea in Control.Moffy.Run.GtkField
	+ [ ] use new Window in Control.Moffy.Run.GtkField
	+ [ ] try to remove old gSignalConnect
	+ [ ] try to remove old etc
	+ [ ] others
* [ ] make independent package gtk-stopgap

### refactoring

* [ ] view module hierarchy
* [ ] refactor module hierarchy
	+ [x] move CairoSurfaceT to CairoType
	+ [x] move Graphics.Gtk.Cairo to Graphics.Cairo
		- [x] move Graphics.Gtk.Cairo.Value to Graphics.Cairo.Value
	+ [x] move Graphics.Gtk.CairoType to Graphics.CairoType
	+ [x] move Graphics.Gtk.Pango to Graphics.Pango
	+ [x] consider whether or not to remove Graphics.Gtk.AsPointer
	+ [ ] others
* [x] refactor Graphics.Gtk.AsPointer
* [x] check export list of Graphics.Pango
	+ [x] consider use ForeignPtr
	+ [x] classify export list
		- [x] Fonts
		- [x] Layout Objects
		- [x] Cairo Fonts and Rendering
		- [x] Types
	+ [x] Basic Pango Interfaces
		- [x] Fonts
		- [x] Layout Objects
		- [x] Types
	+ [x] Rendering with Pango
		- [x] Cairo Fonts and Rendering
* [x] use `PtrForienPtr` (= `Either (Ptr Foo) (ForeignPtr Foo)`)
	+ [x] define `withPtrForeignPtr :: Either (Ptr a) (ForeignPtr a) -> (Ptr a -> IO b) -> IO b`
	+ [x] PangoFontDescription
	+ [x] PangoLayout
	+ [x] CairoSurfaceT
* [x] don\'t use pangoFontDescriptionFromString
	+ [x] define `pangoFontDescriptionSetFamily`
	+ [x] use `pangoFontDescriptionNew` and `pangoFontDescriptionSetFamily` instead of `pangoFontDescriptionFromString`
* [x] don\'t use Text in pangoFontDescriptionFromString
* [x] separate tools for FFI
	+ `type PtrForeignPtr a = Either (Ptr a) (ForeignPtr a)`
	+ `withPtrForeignPtr :: Either (Ptr a) (ForeignPtr a) -> (Ptr a -> IO b) -> IO b`
* [x] check export list of Graphics.Cairo
	+ [x] classify text
	+ [x] remove text
* [x] check export list of Graphics.Cairo.Values
	+ [x] remove text
	+ [x] move Graphics.Cairo.Values into Graphics.Cairo
* [x] check export list of Graphics.Gtk
	+ [x] classify with API reference
		- [x] on the way
		- [x] continue
	+ [x] where delete, destroy and draw event should be classified
* [x] move AsPointer to Foreitn.Tools
* [ ] make gobject-hierarchy like exception-hierarchy
	+ [ ] make life-hierarchy
	+ [ ] others
* [ ] separate Graphics.Gtk
	+ [ ] Graphics.Gtk
	+ [ ] Graphics.Gdk
	+ [x] System.GLib
		- [x] create empty module
		- [x] move from Graphics.Gtk
	+ [x] System.GObject
* [ ] check export list of Graphics.Gtk.Values
* [ ] check export list of Graphics.CairoType
* [ ] check export list of System.Gobject
	+ [ ] consider whether or not to change name of class Event
* [ ] check export list of Foreign.Tools
	+ [ ] consider whether or not to change name of AsPointer
	+ [ ] others
* [ ] consider whether or not to use ForeignPtr to free memory for cairo surface
* [ ] separate Graphics.Pango
	+ Graphics.Pango.Font
	+ Graphics.Pango.Layout
	+ Graphics.Pango.Types
	+ Graphics.Pango.Cairo

### memo

#### Font

* family - ex) courier, serif, sans-serif, Arial, monospace, Sazanami Gothic, Sazanami Mincho
* style - ex) none, normal, italic, oblique
* variant - ex) none, normal, small-caps
* weight - ex) none, normal, bold
* stretch - ex) ultra-condensed, extra-condensed, semi-condensed, normal, semi-expanded
* size
* gravity
* variations

### separate to independent package


Moffy (10)
----------

### module hierarchy

```
Control.Moffy
  +- Control.Moffy.Internal.Sig
  |   +- Control.Moffy.Internal.Sig.Type
  |   +- Control.Moffy.Internal.React
  |   +- Control.Moffy.INternal.React.Type
  +- Control.Moffy.Internal.Sig.Type
  |   +- Control.Moffy.Internal.React.Type
  +- Control.Moffy.Internal.React
  |   +- Control.Moffy.Internal.React.Type
  +- Control.Moffy.Internal.React.Type

Control.Moffy.NoThreadId
  +- Control.Moffy.Internal.Sig
  +- Control.Moffy.Internal.Sig.Type
  +- Control.Moffy.Internal.React
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Handle
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Run
  +- Control.Moffy.Internal.Sig.Type
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Event.ThreadId
  +- Control.Moffy.Internal.React.Type

Control.Moffy.Handle.ThreadId
  +- Control.Moffy.Handle
  +- Control.Moffy.Event.ThreadId
```

### refactoring

* [x] Control.Moffy
	+ [x] API
		- [x] Sig
		- [x] React
		- [x] Constraint
		- [x] Combinator
			* [x] Await and Adjust
			* [x] Create Sig
			* [x] Traverse
			* [x] Parallel
			* [x] Copies
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] PARALLEL
		- [x] COPIES
* [x] Control.Moffy.NoThreadId
	+ [x] API
		- [x] Applicative
		- [x] Parallel
		- [x] Copies
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
* [x] Control.Moffy.Internal.Sig
	+ [x] API
		- [x] Adjust
		- [x] Applicative
		- [x] Parallel
		- [x] Copies
		- [x] Orphan instances
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FLIP APPLICATIVE
			* [x] INSTANCE APPLICATIVE
			* [x] APP AND IAPP
				+ [x] function app_
				+ [x] function exposeBoth
				+ [x] function iapp_
		- [x] PARALLEL
			* [x] AT
			* [x] BREAK AND UNTIL
				+ [x] break_
				+ [x] until_
			* [x] INDEX BY
				+ [x] rename functions
				+ [x] indexBy_
				+ [x] indexByGen
				+ [x] iindexBy
		- [x] COPIES
			* [x] SPAWN
			* [x] PAR LIST
				+ [x] function parList_
				+ [x] function iparList
				+ [x] function cons
		- [x] BASIC COMBINATOR
			* [x] ADJUST
			* [x] PAIRS
			* [x] PAUSE
* [x] Control.Moffy.Internal.Sig.Type
	+ [x] API
		- [x] Type
			* [x] newtype Sig
			* [x] data ISig
			* [x] function isig
		- [x] Function
			* [x] Basic
			* [x] Practical
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] TYPE
		- [x] CLASS INSTANCE
			* [x] MONAD
			* [x] FLIP FUNCTOR
		- [x] FUNCTION
			* [x] BASIC
			* [x] PRACTICAL
* [x] Control.Moffy.Internal.React
	+ [x] API
		- [x] Class
		- [x] Constraint Synonym
		- [x] Function
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] FIRST
		- [x] ADJUST
		- [x] PAR
		- [x] UPDATABLE
* [x] Control.Moffy.Internal.React.Type
	+ [x] API
		- [x] React
			* [x] Type React and Data Rct
			* [x] Class Request
			* [x] Constraint Synonym for Data Occurred
		- [x] Never and Await
		- [x] Hanlde
		- [x] ThreadId
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] REACT
			* [x] TYPE
			* [x] NEVER AND AWAIT
		- [x] CONSTRAINT SYNONYM
		- [x] HANDLE
		- [x] THREAD ID
* [x] Control.Moffy.Handle
	+ [x] API
		- [x] structure
		- [x] Constraint
		- [x] Plain Handle
			* [x] Type
			* [x] Composer
		- [x] Handle with State
			* [x] Type
			* [x] Composer
		- [x] Handle with Input and Output
			* [x] Type
			* [x] Composer
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] CONSTRAINT
		- [x] PLAIN HANDLE
			* [x] TYPE
			* [x] COMPOSER
		- [x] HANDLE WITH STATE
			* [x] TYPE
			* [x] COMPOSER
		- [x] HANDLE WITH INPUT AND OUTPUT
			* [x] TYPE
			* [x] COMPOSER
* [x] Control.Moffy.Run
	+ [x] API
		- [x] Type
		- [x] Run
	+ [x] extension
	+ [x] imports
	+ [x] structure
	+ [x] body
		- [x] SIG
			* [x] interpret
			* [x] interpretSt
				+ ``\h -> (vw h >>) . (`go` st')``
				+ ``\h -> (`go` st') >>> (vw h >>)``
				+ ``(. (`go` st')) . (>>) . vw``
				+ ``((`go` st') >>>) . (>>) . vw``
		- [x] REACT
			* [x] interpretReact
			* [x] interpretReactSt
			* [x] runSt
* [x] Control.Moffy.Event.ThreadId
	+ [x] API
		- [x] GetThreadId
		- [x] ThreadId
	+ [x] extension
	+ [x] imports
	+ [x] body
* [x] Control.Moffy.Handle.ThreadId
	+ [x] API
	+ [x] extension
	+ [x] imports
	+ [x] body

## Moffy library (20)

### module hierarchy

```
Control.Moffy.Event
  + Control.Moffy.Event.Lock
	+ Control.Moffy.Event.Lock.Internal
  + Control.Moffy.Event.Random
	+ Control.Moffy.Event.Random.Internal
  + Control.Moffy.Event.Time
  + Control.Moffy.Event.Delete
  + Control.Moffy.Event.Key
      + Control.Moffy.Event.Key.Internal.XK
          + Control.Moffy.Event.Key.Internal
      + Control.Moffy.Event.Key.Internal
  + Control.Moffy.Event.Mouse
  + Control.Moffy.Event.CalcTextExtents
Control.Moffy.Handle
  + Control.Moffy.Handle.Lock
  + Control.Moffy.Handle.Random
  + Control.Moffy.Handle.Time
  + Control.Moffy.Handle.XField
      + Control.Moffy.Handle.XField.Key
      + Control.Moffy.Handle.XField.Mouse
      + Control.Moffy.Handle.XField.CalcTextExtents
  + Control.Moffy.Handle.GtkField
```

### refactor modules

* [x] Control.Moffy.Event.Lock
	+ [x] API
		- [x] Type
		- [x] Event
	+ [x] import
* [x] Control.Moffy.Event.Lock.Internal
	+ [x] API
		- [x] Type Synonym
		- [x] Event Type
		- [x] Event
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] LOCK ID
		- [x] EVENT
			* [x] NEW LOCK ID
			* [x] GET LOCK
			* [x] UNLOCK
		- [x] WITH LOCK
* [x] Control.Moffy.Handle.Lock
	+ [x] API
		- [x] Type
		- [x] Handle
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] LOCK STATE
		- [x] HANDLE
			* [x] handleLock
			* [x] handleNewLockId
			* [x] handleGetLock
			* [x] handleUnlock
* [x] Control.Moffy.Event.Random
	+ [x] API
		- [x] Type
		- [x] Get Random
	+ [x] import
* [x] Control.Moffy.Event.Random.Internal
	+ [x] API
		- [x] Store Random Gen
		- [x] Load Random Gen
		- [x] Get Random
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] STORE RANDOM GEN
		- [x] LOAD RANDOM GEN
		- [x] RANDOM EV AND GET RANDOM
* [x] Control.Moffy.Handle.Random
	+ [x] API
		- [x] Type
			* [x] type RandomEv
			* [x] class RandomState
		- [x] Handle
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] RANDOM STATE
		- [x] HANDLE
* [x] Control.Moffy.Event.Time
	+ [x] API
		- [x] structure
		- [x] Time Event
		- [x] Elapsed
			* [x] data DeltaTime
			* [x] pattern OccDeltaTime
			* [x] react deltaTime
			* [x] sig elapsed
		- [x] Sleep
			* [x] newtype TryWait
			* [x] pattern OccTryWait
			* [x] function sleep
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] ELAPSED
		- [x] SLEEP
		- [x] TIME EVENT
* [x] Control.Moffy.Handle.Time
	+ [x] consider whether or not to rename Mode
	+ [x] rename WaitMode to FlushWaitMode
	+ [x] API
		- [x] structure
		- [x] TimeState
			* [x] class TimeState
			* [x] data Mode
		- [x] IO Mimicable
			* [x] TaiTimeM
			* [x] DelayM
		- [x] Handle
		- [x] define Timable
	+ [x] extension
	+ [x] import
	+ [x] structure and body
	+ [x] body
		- [x] TIME STATE
		- [x] IO MIMICABLE
		- [x] HANDLE
			* [x] constraint synonym Timable
			* [x] handleTimeEvPlus
			* [x] handleI
			* [x] handleF
			* [x] handleTime
* [x] Control.Moffy.Event.Delete
	+ [x] API
		- [x] Type
		- [x] Event
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Control.Moffy.Event.Key
	+ [x] API
		- [x] Key Event
		- [x] Key Down Event
		- [x] Key Up Event
		- [x] Key
		- [x] module Control.Moffy.Event.Key.Internal.XK
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] EVENT
			* [x] KEY DOWN
			* [x] KEY UP
			* [x] KEY EVENT
		- [x] PATTERN
* [ ] Control.Moffy.Event.Key.Internal
	+ [ ] API
		- [x] Type
		- [ ] Template
			* [ ] Void Symbol
			* [ ] Tty Function Keys
			* [ ] Japanese Keyboard
			* [ ] Cursor Control & Motion
			* [ ] Misc Function
			* [ ] Auxiliary functions: F1 to F35
			* [ ] Modifiers
			* [ ] Keyboard (XKB) Extension Function and Modifier Keys
			* [ ] Latin 1 (only Ascii)
	+ [ ] extension
	+ [ ] import
	+ [ ] structure
	+ [ ] body
		- [ ] TYPE AND AUXILIARY FUNCTION
		- [ ] DEFINITION OF PATTERN XK_FOO
			* [ ] VOID SYMBOL
			* [ ] TTY FUNCTION KEYS
			* [ ] JAPANESE KEYBOARD SUPPORT
			* [ ] CURSOR CONTROL AND MOTION
			* [ ] MISC FUNCTION
			* [ ] AUXILIARY FUNCTION
			* [ ] MODIFIERS
			* [ ] KEYBOARD (XKB) EXTENSION FUNCTION AND MODIFIER KEYS
			* [ ] LATIN 1 (only ASCII)
* [ ] Control.Moffy.Event.Key.Internal.XK
	+ [ ] API
		- [ ] Void Symbol
		- [ ] Tty Function Keys
		- [ ] Japanese Keyboard
		- [ ] Cursor Control & Motion
		- [ ] Misc Function
		- [ ] Ausiliary Functions: F1 to F35
		- [ ] Modifiers
		- [ ] Keyboard (Xkb) Extension Function and Modifier Keys
		- [ ] Latin 1 (Only Ascii)
			* [ ] Space to Slash
			* [ ] Digit
			* [ ] Colon to At
			* [ ] Upper Alphabet
			* [ ] Blacketleft to Grave
			* [ ] Lower Alphabet
			* [ ] Braceleft to Asciitilde
	+ [ ] extension
	+ [ ] import
	+ [ ] structure
	+ [ ] body
		- [ ] VOID SYMBOL
		- [ ] NOT VISIBLE
		- [ ] ASCII
* [ ] Control.Moffy.Event.Mouse
	+ [ ] consider whether or not to unlist [MouseBtn]
	+ [ ] API
		- [ ] structure
		- [ ] Type
			* [ ] MouseEv
			* [ ] MouseBtn
				+ [ ] rename and add buttons
				+ [ ] others
			* [ ] Point
		- [ ] Mouse Down
			* [ ] MouseDown
			* [ ] OccMouseDown
			* [ ] mouseDown
			* [ ] leftClick
			* [ ] middleClick
			* [ ] rightClick
		- [ ] Mouse Up
			* [ ] MouseUp
			* [ ] OccMouseUp
			* [ ] mouseUp
			* [ ] leftUp
			* [ ] middleUp
			* [ ] rightUp
		- [ ] Mouse Move
			* [ ] MouseMove
			* [ ] OccMouseMove
			* [ ] mouseMove
	+ [ ] extension
	+ [ ] import
	+ [ ] structure
	+ [ ] body
		+ [ ] MOUSE DOWN
			- [ ] data MouseDown
			- [ ] numbered MouseDown
			- [ ] instance Request MouseDown
			- [ ] data MouseBtn
			- [ ] function mouseDown
			- [ ] function clickOn
			- [ ] function leftClick, middleClick, rightClick
		+ [ ] MOUSE UP
			- [ ] data MouseUp
			- [ ] numbered MouseUp
			- [ ] instance Request MouseUp
			- [ ] function mouseUp
			- [ ] function releaseOn
			- [ ] function leftUp, middleUp, rightUp
		+ [ ] MOUSE MOVE
			- [ ] data MouseMove
			- [ ] numbered MouseMove
			- [ ] instance Request MouseMove
			- [ ] type Point
			- [ ] function mouseMove
		+ [ ] MOUSE EV
* [ ] Control.Moffy.Handle.XField
	+ [ ] API
		- [ ] structure
		- [ ] Type
		- [ ] Handle
			* [ ] function handle
			* [ ] function handleWith
	+ [ ] extension
	+ [ ] import
	+ [ ] structure
	+ [ ] body
		- [ ] GUI EV
		- [ ] HANDLE
			* [ ] function handle
			* [ ] function handleWith
			* [ ] function eventToEv
* [ ] Control.Moffy.Handle.XField.Key
	+ [ ] API
	+ [ ] extension
	+ [ ] import
	+ [ ] body
* [ ] Control.Moffy.Handle.XField.Mouse
	+ [ ] API
	+ [ ] extension
	+ [ ] import
	+ [ ] body
* [ ] Control.Moffy.Event.CalcTextExtents
	+ [ ] rename
	+ [ ] API
	+ [ ] extension
	+ [ ] import
	+ [ ] body
* [ ] Control.Moffy.Handle.GtkField
	+ [ ] rename
	+ [ ] API
	+ [ ] extension
	+ [ ] import
	+ [ [ body

Moffy Base (9)
----------

### Control.Monad.Freer.Par (6)

#### module hierarchy

```
Control.Monad.Freer.Par
  +- Control.Monad.Freer.Par.Sequence
  +- Control.Monad.Freer.Par.Funable
  |    +- Control.Monad.Freer.Par.Internal.Id
  +- Control.Monad.Freer.Par.Internal.Id

Control.Monad.Freer.Par.FTCQueue
  +- Control.Monad.Freer.Par.Sequence

Control.Monad.Freer.Par.TaggableFunction
  +- Control.Monad.Freer.Par.Funable
```

#### repair

* [x] repair tag system
	+ [x] make package try-freer-par
	+ [x] copy from package try-freer-par

#### refactoring

* [x] Control.Monad.Freer.Par
	+ [x] API
		- [x] rename Unique
		- [x] consider whether or not to rename qApp and qAppPar
			* qApp -> app
			* qAppPar -> appPar
		- [x] structure
		- [x] Freer
			* [x] Type
				+ [x] data Freer
				+ [x] data Fun
			* [x] Pattern
				+ [x] pattern Pure
				+ [x] pattern (:>>=)
				+ [x] pattern (:=<<)
			* [x] Bind
				+ [x] operator (>>>=)
				+ [x] operator (=<<<)
			* [x] Apply
				+ [x] function app
				+ [x] function appPar
		- [x] Tagged
			* [x] data Tagged
			* [x] function runTagged
			* [x] function tag
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] consider whether or not add fixity of (::>>=), (<|) and (|>)
	+ [x] consider whether or not add fixity of (>>>=) and (=<<<)
	+ [x] body
		- [x] PARALLEL FREER
			* [x] TYPE AND MONAD
				+ [x] data Freer
				+ [x] function freer
				+ [x] instance Functor
				+ [x] instance Applicative
				+ [x] instance Monad
				+ [x] newtype Fun
			* [x] PATTERN
				+ [x] pattern Pure
				+ [x] pattern (:>>=)
				+ [x] pattern (:=<<)
			* [x] BIND
				+ [x] operator (>>>=)
				+ [x] operator (=<<<)
			* [x] APPLICATION
				+ [x] function app
				+ [x] function appPar
				+ [x] function aps
				+ [x] function aps'
				+ [x] function apsPar
		- [x] TAGGED
			* [x] newtype Tagged
			* [x] instance Functor
			* [x] instance Applicative
			* [x] instance Monad
			* [x] function runTagged
			* [x] function tag
* [x] Control.Monad.Freer.Par.Sequence
	+ [x] API
		- [x] structure
		- [x] Sequence and ViewL
		- [x] Combinator
	+ [x] extension
	+ [x] structure
	+ [x] body
		- [x] SEQUENCE AND VIEWL
		- [x] COMBINATOR
* [x] Control.Monad.Freer.Par.Funable
	+ [x] API
		- [x] structure
		- [x] Funable
		- [x] Taggalble
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] class Funable
		- [x] class Taggable
		- [x] data Tag
		- [x] function sameTag
* [x] Control.Monad.Freer.Par.Internal.Id
	+ [x] API
	+ [x] import
	+ [x] body
* [x] Control.Monad.Freer.Par.FTCQueue
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Control.Monad.Freer.Par.TaggableFunction
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] data TaggableFun
		- [x] instance Funable TaggableFun
		- [x] instance Taggable TaggableFun

### One Or More (1)

#### module hierarchy

```
Data.OneOrMore
```

#### refactor

* [x] Data.OneOrMore
	+ [x] API
		- [x] structure
		- [x] Type
		- [x] Property
			* [x] Basic Property
				+ [x] Projectable
				+ [x] Insertable
			* [x] Expandable and Collapsable
				+ [x] Expandable
				+ [x] Collapsable
			* [x] Mergeable
				+ [x] Mergeable
				+ [x] Selectable
		- [x] Function
			* [x] Single Type
				+ [x] pattern Singleton
				+ [x] function unSingleton
			* [x] Multiple Type
				+ [x] function project
				+ [x] operator (>-)
			* [x] Expand and Collapse
				+ [x] expand
				+ [x] collapse
			* [x] Merge
				+ [x] merge
				+ [x] merge'
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] ONE OR MORE TYPE
		- [x] BASIC PROPERTY
			* [x] PROJECTABLE
				+ [x] class Projectable
				+ [x] pattern Singleton
				+ [x] function unSingleton
			* [x] INSERTABLE
		- [x] EXPANDABLE AND COLLAPSABLE
			* [x] EXPANDABLE
				+ [x] class Expandable
				+ [x] class Nihil
			* [x] COLLAPSABLE
				+ [x] COLLAPSABLE 0
				+ [x] COLLAPSABLE
		- [x] MERGEABLE
			* [x] class Mergeable
			* [x] class Selectable
			* [x] function merge'

### Type Set (2)

#### module hierarchy

```
Data.Type.Set
  +- Data.Type.Set.Internal
```

#### refactor

* [x] Data.Type.Set
	+ [x] API
		- [x] structure
		- [x] Set
		- [x] Numbered
		- [x] Function
		- [x] Operator
	+ [x] import
* [x] Data.Type.Set.Internal
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TYPE SET
			* [x] DATA DEFINITION
			* [x] COMBINATOR
				+ [x] Singleton
				+ [x] Insert
				+ [x] Merge
				+ [x] Map
		- [x] NUMBERED
		- [x] BOOL

Trials (20)
------

### tribial (10)

#### TrySharing

##### repair

* [x] use showButton
* [x] remove old functions

##### addition

* [x] add nest first trial

##### refactoring

* [x] Trial.TrySharing
	+ [x] rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] NO SHARING
		- [x] SHARING
			* [x] SIMPLE
			* [x] NEST FIRST'
				+ [x] function runSharingShowButton4
				+ [x] function runSharingShowButton8
			* [x] TWO TIME CLICK
		- [x] TOOLS
			* [x] TYPE OR'
			* [x] RUN MOUSE EV
			* [x] SHOW BUTTON
				+ [x] function showButton
				+ [x] function show'
				+ [x] message

#### CheckSharing.EvInt

##### refactoring

* CheckSharing.EvInt
	+ [x] remove it

#### TrySharing.ThreadId

* [x] TrySharing.ThreadId
	+ [x] view
	+ [x] rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TRIAL
			* [x] action runFirstGetThreadId
			* [x] action runSharingFirstGetThreadId
			* [x] action runSharingFirstGetThreadId'
		- [x] PARTS
			* [x] type Or'
			* [x] runMouseThreadId
			* [x] react heavyGetThreadId
			* [x] react heavyMouseDown
			* [x] function heavyId

#### Count

##### refactoring

* [x] Trial.Count
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] TRIAL
			* [x] action tryLeftCount
			* [x] action tryLeftCountSig
			* [x] action tryLeftRandomSig
		- [x] REACT AND SIG
			* [x] function leftCount
			* [x] function leftCountSig
			* [x] function leftRandomSig
		- [x] RUN
			* [x] runMouseReact
			* [x] runMouse

#### Try Key

* [x] Trial.TryKey
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] action tryKey
		- [x] sig keySig
		- [x] react asciiKey
		- [x] react asciiKeyUp

#### Try ThreadId

* [x] Trial.ThreadId
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
		- [x] trySingleThreadId
		- [x] tryDoubleThreadId
		- [x] runGetThreadId
		- [x] tryLeftRightThreadId
		- [x] tryLeftRightThreadId'
		- [x] runMouseGetThreadId
		- [x] clickThenGetThreadId

#### Try Lock

* [x] Trial.TryLock
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] LOCK ST
			* [x] data LockSt
			* [x] instance LockState LockSt
		- [x] TRIAL
			* [x] SINGLE
				+ [x] action trySingleLeftCount
				+ [x] function leftCount
			* [x] NO LOCK
			* [x] LOCK
				+ [x] action tryLockLeftCount2
				+ [x] function lockLeftCount
		- [x] RUN
			* [x] function runClick
			* [x] function runClickLockSt

#### Try Random

##### refactoring

* [x] Trial.TryRandom
	+ [x] view
	+ [x] consider whether or not to rename
	+ [x] API
	+ [x] import
	+ [x] body
		- [x] list diceTrial
		- [x] function evalRandom
		- [x] function getRandomRs

#### Count With Lock

* [x] Trial.CountWithLock
	+ [x] view
	+ [x] consider whether or not to remove

### Boxes (6)

#### module hierarchy

```
Main
  +- Trial.Boxes
  |    +- Trial.Boxes.Box
  |    +- Trial.Boxes.BoxEv
  +- Trial.Boxes.Run
       +- Trial.Boxes.Handle
       |    +- Trial.Boxes.BoxEv
       +- Trial.Boxes.View
            +- Trial.Boxes.Box
```

#### refactoring

* [x] refactor module name and hierarchy
* [x] check module hierarchy
* [x] refactor each modules

##### module Trial.StepByStepBox

* [x] view
* [x] separate to Trial.Boxes and other
	+ [x] create empty module Trial.Boxes
	+ [x] others
* [x] separate to Trial.Boxes and other
	+ [x] create empty module Trial.Boxes.View
	+ [x] others
* [x] rename to Trial.Boxes.Run
	+ [x] rename
	+ [x] API
* [x] repair app/Main.hs

##### each modules

* [x] Main
	+ [x] import
	+ [x] body
		- [x] rename trySigGBoxes' to runBoxes
* [x] Trial.Boxes
	+ [x] view
	+ [x] consider
		- [x] whether or not to move elapsed to Control.Moffy.Event.Time
		- [x] whether or not to move mousePos to Control.Moffy.Event.Mouse
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] BOXES
			* [x] sig boxes
			* [x] sig box
		- [x] DEFINE RECT
			* [x] sig defineRect
			* [x] react firstPoint
			* [x] function completeRect
			* [x] value neverOccur
		- [x] CHOOSE BOX COLOR
			* [x] function chooseBoxColor
			* [x] function wiggleRect
			* [x] sig cycleColor
		- [x] DR CLICK ON
			* [x] function drClickOn
			* [x] react doubler
		- [x] BEFORE
	+ [x] import
* [x] Trial.Boxes.Box
	+ [x] API
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.BoxEv
	+ [x] API
		- [x] rename SigG, ISigG and ReactG to SigB, ISigB and ReactB
		- [x] others
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.Run
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.Handle
	+ [x] API
	+ [x] import
	+ [x] body
* [x] Trial.Boxes.View
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] drawBoxes
		- [x] drawBox
		- [x] drawREct
		- [x] withFlush
* [x] consider unify module Trial.Boxes.Run and Trial.Boxes.Handle
* [x] Trial.Boxes.Run
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] body

### Try Check Dup

* [x] Trial.TryCheckDup
	+ [x] consider whether or not to remove
		- [x] remove doubler from export list of Trial.Boxes

### Followbox (8)

#### module hierarchy

```
Main
  +- Trial.Followbox
  |    +- Trial.Followbox.Event
  |    |    +- Trial.Followbox.TypeSynonym
  |    +- Trial.Followbox.Clickable
  |    |    +- Trial.Followbox.Event
  |    |    +- Trial.Followbox.ViewType
  |    |    +- Trial.Followbox.TypeSynonym
  |    +- Trial.Followbox.ViewType
  |    |    +- Trial.Followbox.TypeSynonym
  |    +- Trial.Followbox.TypeSynonym
  +- Trial.Followbox.Run
       +- Trial.Followbox.Handle
       |    +- Trial.Followbox.Event
       |    +- Trial.Followbox.TypeSynonym
       +- Trial.Followbox.View
       |    +- Trial.Followbox.ViewType
       +- Trial.Followbox.TypeSynonym
```

#### refactoring

* [x] refactor module dependencies
	+ [x] make module Trial.Followbox not to depend Trial.Followbox.View
		- [x] separate Trial.Followbox.View to itself and Trial.Followbox.ViewType
		- [x] Trial.Followbox import ....ViewType instead of ...View
	+ [x] rename Trial.Followbox.HandleNew to ...Handle
* [x] Main
	+ [x] import
	+ [x] body
* [x] Trial.Followbox
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] PARAMETER LIST
			* [x] view
			* [x] NUMBER OF USER TO DISPLAY
			* [x] MAX NUMBER OF GITHUB USER
			* [x] BACKGROUND
				+ [x] titlePos
				+ [x] nextPos
				+ [x] refreshPos
				+ [x] resetTimePos
			* [x] FONT
				+ [x] defaultFont
				+ [x] middleSize, largeSize
			* [x] AVATAR, NAME AND CROSS
				+ [x] avatarSizeX, avatarSizeY
				+ [x] avatarPos, namePos
				+ [x] crossSize
				+ [x] crossPos
				+ [x] crossMargin
		- [x] SIG AND REACT
			* [x] FOLLOWBOX
				+ [x] followbox
				+ [x] fieldWithResetTime
				+ [x] field
				+ [x] resetTime
			* [x] USERS
				+ [x] users
				+ [x] user1
				+ [x] cross
			* [x] GET USER
				+ [x] getUser
				+ [x] getAvatar
			* [x] GET OBJECT
				+ [x] getObj1
				+ [x] getObj1FromWeb
				+ [x] getObjs
		- [x] HELPER FUNCTION
			* [x] twhite
			* [x] lwhite
			* [x] posixSeconds
* [x] Trial.Followbox.Clickable
	+ [x] API
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] consider to change view and click to field
		- [x] CLICKABLE
			* [x] data Clickable
			* [x] clickable
		- [x] WITH TEXT EXTENTS
			* [x] data WithTextExtents
			* [x] clickableText
			* [x] withTextExtents
			* [x] nextToText
			* [x] translate
* [x] Trial.Followbox
	+ [x] body
		- [x] SIG AND REACT: FOLLOWBOX: function field
		- [x] SIG AND REACT: USERS: function user1
* [x] Trial.Followbox.Event
	+ [x] API
		- [x] structure
		- [x] Followbox Event
			* [x] type SigF
			* [x] type ReactF
			* [x] type FollowboxEv
		- [x] Store and Load Jsons
			* [x] data StoreJsons
			* [x] pattern OccStoreJsons
			* [x] data LoadJsons
			* [x] pattern OccLoadJsons
			* [x] clearJsons
			* [x] storeJsons
			* [x] loadJsons
		- [x] Request Data
			* [x] Http Get
				+ [x] data HttpGet
				+ [x] pattern OccHttpGet
				+ [x] httpGet
			* [x] Calc Text Extents
				+ [x] data CalcTextExtents
				+ [x] pattern OccCalcTextExtents
				+ [x] calcTextExtents
			* [x] Get Time Zone
				+ [x] data GetTimeZone
				+ [x] pattern OccGetTimeZone
				+ [x] getTimeZone
		- [x] Browse
			+ [x] data Browse
			+ [x] pattern OccBrowse
			+ [x] browse
		- [x] Sleep
			+ [x] data BeginSleep
			+ [x] pattern OccBeginSleep
			+ [x] data EndSleep
			+ [x] pattern OccEndSleep
			+ [x] beginSleep
			+ [x] checkBeginSleep
			+ [x] endSleep
		- [x] Raise Error
			+ [x] data RaiseError
			+ [x] pattern OccRaiseError
			+ [x] data Error
			+ [x] data ErrorResult
			+ [x] raiseError
			+ [x] checkTerminate
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] STORE AND LOAD JSON OBJECT LIST
			* [x] define and instance Request of StoreJsons
			* [x] function clearJsons
			* [x] function storeJsons
			* [x] define and instance Request of LoadJsons
			* [x] functin loadJsons
		- [x] REQUEST DATA
			* [x] HTTP GET
				+ [x] define and instance Request of HttpGet
				+ [x] function httpGet
			* [x] CALC TEXT EXTENTS
				+ [x] define and instance Request of CalcTextExtents
				+ [x] function calcTextExtents
			* [x] TIME ZONE
				+ [x] define and instance Request of GetTimeZone
				+ [x] function getTimeZone
		- [x] BROWSE
			* [x] define and instance Request of Browse
			* [x] function browse
		- [x] SLEEP
			* [x] define and instance Request of BeginSleep
			* [x] function beginSleep
			* [x] function checkBeginSleep
			* [x] define and instance Request of EndSleep
			* [x] function endSleep
		- [x] RAISE ERROR
			* [x] data Error
			* [x] data ErrorResult
			* [x] define and instance Request of RaiseError
			* [x] function raiseError
			* [x] function catchError
			* [x] function checkTerminate
		- [x] FOLLOWBOX EVENT TYPE
			* [x] type SigF
			* [x] type ReactF
			* [x] type FollowboxEv
* [x] Trial.Followbox.TypeSynonym
	+ [x] API
		- [x] structure
		- [x] rename from FOO to Foo
		- [x] Field
			* [x] WindowTitle
			* [x] Position
			* [x] LineWidth
			* [x] FontName
			* [x] FontSize
			* [x] Avatar
		- [x] GitHub
			* [x] GithubNameToken
			* [x] GithubUserName
			* [x] GithubToken
		- [x] Others
			* [x] Uri
			* [x] Browser
			* [x] ErrorMessage
	+ [x] import
	+ [x] structure
	+ [x] body
* [x] Trial.Followbox.Run
	+ [x] API
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] DEFAULt BROWSER
		- [x] RUN FOLLOWBOX
			* [x] runFollowbox
			* [x] evalFollowbox
			* [x] run
		- [x] GET FOLLOWBOX INFO
			* [x] data FollowboxInfo
			* [x] getFollowboxInfo
			* [x] optionToInfo
		- [x] GET OPT
			* [x] data FollowboxOption
			* [x] chkDupOpt
			* [x] followboxOptions
* [x] Trial.Followbox.Handle
	+ [x] API
		- [x] structure
		- [x] Handle
			* [x] type HandleF
			* [x] handleFollowbox
		- [x] State
			* [x] data FollowboxState
			* [x] initialFollowboxState
	+ [x] extension
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] STATE
			* [x] FOLLOWBOX STATE
				+ [x] data FollowboxState
				+ [x] initialFollowboxState
				+ [x] type HandleF
				+ [x] type HandleF'
			* [x] PUT AND GET EACH STATE
				+ [x] instance LockState FollowboxState
				+ [x] instance RandomState FollowboxState
		- [x] HANDLE
			* [x] FOLLOWBOX
			* [x] MOUSE
			* [x] STORE AND LOAD JSONS
			* [x] REQUEST DATA
				- [x] handleHttpGet
				- [x] handleCalcTextExtents
				- [x] handleGetTimeZone
			* [x] BROWSE
			* [x] BEGIN AND END SLEEP
			* [x] RAISE ERROR
		- [x] HELPER FUNCTION
* [x] Trial.Followbox.View
	+ [x] API
		- [x] View
			* [x] type View
			* [x] data View1
			* [x] function view
		- [x] Color
			* [x] data Color
			* [x] value white
			* [x] value blue
	+ [x] import
	+ [x] structure
	+ [x] body
		- [x] VIEW
			* [x] view
			* [x] view1
				+ [x] match Text
				+ [x] match Line
				+ [x] match Image
			* [x] colorToPixel
		- [x] DRAW PIXEL IMAGE
			* [x] drawImagePixel
			* [x] swap02s
* [x] Trial.Followbox.ViewType

ref
---

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```

```
f :: Integer -> Integer -> Rational
f (fromInteger . (2 ^) -> m) n = 1 - product [m - fromInteger n + 1 .. m] / m ^ n

> fromRational . recip $ f 64 10000 :: Double
3.6897177865255615e11
```

task
----
