memo
====

structure
---------

* Basic Pango Interfaces
	+ Rendering
	+ Glyph Storage
	+ Fonts
		- PangoFontDescription
		- others
	+ Text Attributes
	+ Tab Stops
	+ Text Attribute Markup
	+ Layout Objects
	+ Scripts and Languages
	+ Bidirectional Text
	+ Vertical Text
* Rendering with Pango
	+ Win32 Fonts and Rendering
	+ FreeType Fonts and Rendering
	+ Xft Fonts and Rendering
	+ Cairo Rendering
	+ CoreText Fonts
* Low Level Functionality
	+ PangoRenderer
	+ PangoFcFontMap
	+ PangoFcFont
	+ PangoFcDecoder
	+ OpenType Font Handling
	+ Coverage Maps
	+ Engines
	+ PangoEngineLang
	+ PangoEngineShape
	+ Modules
	+ Miscellaneous Utilities
	+ Version Checking

todo
----

* [ ] organize about PrimMonad, IO and freeze
	+ [x] remove branch tmp
	+ [x] make branch: prim-and-io
	+ [ ] PangoContext and PangoLayout should use `IO` instead of `PrimMonad m => m`
		- [x] pangoCairoCreateContext should use `IO`
		- [x] change from `PangoContext s` to `PangoContext`
		- [x] pangoCairoCreateLayout should use `IO`
		- [x] pangoCairoShowLayout should use `IO`
		- [ ] change from `PangoLayout s` to `pangoLayout`
	+ [ ] others
* [ ] rename `PangoFontDescription` to `PangoFontDescriptionPrim`
* [ ] define `PangoFontDescription` and freezer
* [ ] Basic Pango Interfaces
	+ [x] Fonts
		- [x] PangoFontDescription
	+ [ ] LayoutObjects
	+ [ ] Text Attributes
	+ [ ] Text Attribute Markup
	+ [ ] Tab Stops
	+ [ ] Bidirectional Text
	+ [ ] Vertical Text
	+ [ ] Scripts and Languages
* [ ] Rendering with Pango
	+ [ ] Cairo Rendering
* [ ] think about package name
	+ simple-cairo-pango ?

not do it yet
-------------

* Basic Pango Interfaces
	+ Rendering
	+ Fonts
		- others
	+ Glyph Storage
* Rendering with Pango
	+ Win32 Fonts and Rendering
	+ FreeType Fonts and Rendering
	+ Xft Fonts and Rendering
	+ CoreText Fonts
* Low Level Functionality

simple usage
------------

```haskell
foo = do
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetFoo ...
	...
	pfd' <- pangoFontDescriptionFreeze pfd
	pangoLayoutSetFontDescription pl pfd'
	pangoLayoutSetText pl "Foo Bar"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
```

* Basic Pango Interfaces
	+ Fonts
		- [ ] pangoFontDescriptionNew
		- [ ] pangoFontDescriptionSetFoo
		- [ ] pangoFontDescriptionFreeze
	+ Layout Objects
		- [ ] pangoLayoutSetFontDescription
		- [ ] pangoLayoutSetText
		- [ ] pangoLayoutFreeze
* Rendering with Pango
	+ Cairo Rendering
		- [ ] pangoCairoCreateLayout
		- [ ] pangoCairoShowLayout

old
---

### todo

* [x] use IO instead of PrimMonad m => m
	+ [x] pango\_font\_description\_new
	+ [x] pango\_font\_description\_copy
	+ [x] pango\_font\_description\_copy\_static
	+ no pango\_font\_description\_hash
	+ [x] pango\_font\_description\_equal
	+ [x] pango\_font\_description\_free
		- use in newForeignPtr
	+ no pango\_font\_descriptions\_free
	+ [x] pango\_font\_description\_set\_family
	+ [x] pango\_font\_description\_set\_family\_static
	+ [x] pango\_font\_description\_get\_family
	+ [x] pango\_font\_description\_set\_style
	+ [x] pango\_font\_description\_get\_style
	+ [x] pango\_font\_description\_set\_variant
	+ [x] pango\_font\_description\_get\_variant
	+ [x] pango\_font\_description\_set\_weight
	+ [x] pango\_font\_description\_get\_weight
	+ [x] pango\_font\_description\_set\_stretch
	+ [x] pango\_font\_description\_get\_stretch
	+ [x] pango\_font\_description\_set\_size
	+ [x] pango\_font\_description\_get\_size
	+ [x] pango\_font\_description\_set\_absolute\_size
* [x] change PangoFontDescription: from * -> * to *
* [x] change PangoLayout: from * -> * to *
* [x] remove Graphics.Pango.Monad
* [x] make Graphics.Pango.Monad
* [x] make PangoLayoutIo and PangoLayout
	+ [x] rename PangoLayout to PangoLayoutOld
	+ [x] rename makePangoLayout to makePangoLayoutOld
	+ [x] make PangoLayoutIo
		- [x] make PangoLayoutIo
		- [x] use PangoLayoutIo
	+ [x] make PangoLayout
		- [x] make PangoLayout
		- [x] use PangoLayout
		- [x] pangoCairoShowLayout: from IO to PrimMonad m => m
	+ [x] make pangoLayoutFreeze
	+ [x] make pangoLayoutThaw
	+ [x] remove PangoLayoutOld and so on
* [x] make PangoFontDescriptionPrim and PangoFontDescription
	+ [x] rename PangoFontDescription to PangoFontDescriptionOld
	+ [x] make PangoFontDescriptionPrim
		- [x] make PangoFontDescriptionPrim
		- [x] use PangoFontDescriptionPrim
			* [x] pangoFontDescriptionNew
			* [x] pangoFontDescriptionCopy
			* [x] pangoFontDescriptionCopyStatic
			* [x] pangoFontDescriptionSetFamily
			* [x] pangoFontDescriptionSetFamilyStatic
			* [x] pangoFontDescriptionSetStyle
			* [x] pangoFontDescriptionSetVariant
			* [x] pangoFontDescriptionSetWeight
			* [x] pangoFontDescriptionSetStretch
			* [x] pangoFontDescriptionSetSize
			* [x] pangoFontDescriptionSetAbsoluteSize
			* [x] pangoFontDescriptionSetGravity
			* [x] pangoFontDescriptionUnsetFields
	+ [x] make PangoFontDescription
		- [x] make PangoFontDescription
		- [x] repair about PangoFontDescription: not IO
			* [x] pangoFontDescriptionEqual
			* [x] pangoFontDescriptionGetFamily
			* [x] pangoFontDescriptionGetStyle
			* [x] pangoFontDescriptionGetVariant
			* [x] pangoFontDescriptionGetWeight
			* [x] pangoFontDescriptionGetStretch
			* [x] pangoFontDescriptionGetSize
			* [x] pangoFontDescriptionGetSizeIsAbsollute
			* [x] pangoFontDescriptionGetGravity
			* [x] pangoFontDescriptionGetSetFields
			* [x] pangoFontDescriptionToString
			* [x] pangoFontDescriptionToFilename
	+ [x] make pangoFontDescriptionFreeze
	+ [x] pangoLayoutSetFontDescription: use PangoFontDescription
	+ [x] make pangoFontDescriptionThaw
	+ [x] remove PangoFontDescriptionOld and so on
* [ ] define functions of Fonts
	+ [x] pango\_font\_description\_get\_size\_is\_absolute
	+ [x] pango\_font\_description\_set\_gravity
	+ [x] pango\_font\_description\_get\_gravity
	+ [ ] pango\_font\_description\_set\_variations
	+ [ ] pango\_font\_description\_set\_variations\_static
	+ [ ] pango\_font\_description\_get\_variations
	+ [x] pango\_font\_description\_get\_set\_fields
	+ [x] pango\_font\_description\_unset\_fields
	+ no pango\_font\_description\_merge
	+ no pango\_font\_description\_merge\_static
	+ no pango\_font\_description\_better\_match
	+ no pango\_font\_description\_from\_string
	+ [x] pango\_font\_description\_to\_string
	+ [x] pango\_font\_description\_to\_filename
	+ pango\_font\_metrics\_ref
	+ pango\_font\_metrics\_unref
	+ pango\_font\_metrics\_get\_ascent
	+ pango\_font\_metrics\_get\_descent
	+ pango\_font\_metrics\_get\_height
	+ ...
	+ no pango\_font\_find\_shaper
	+ no pango\_font\_describe
	+ ...
	+ no pango\_font\_family\_get\_name
	+ ...
	+ no pango\_font\_face\_get\_face\_name
	+ ...
	+ pango\_font\_map\_create\_context
	+ no pango\_font\_map\_load\_font
	+ ...
	+ pango\_font\_map\_changed
	+ no pango\_fontset\_get\_font
	+ pango\_font\_set\_get\_metrics
	+ no pango\_font\_set\_foreach
* [ ] define function of pango layout objects
	+ [x] pango\_layout\_new
	+ [x] pango\_layout\_copy
	+ [ ] pango\_layout\_get\_context
		- use g_object_ref
	+ [ ] pango\_layout\_context\_changed
	+ [ ] pango\_layout\_get\_serial
	+ [x] pango\_layout\_set\_text
	+ [x] pango\_layout\_get\_text
	+ [ ] pango\_layout\_get\_character\_count
	+ no pango\_layout\_set\_markup
	+ no pango\_layout\_set\_markup\_with\_accell
	+ no pango\_layout\_set\_attributes
	+ no pango\_layout\_get\_attributes
	+ [x] pango\_layout\_set\_font\_description
	+ [ ] pango\_layout\_get\_font\_description
		+ make PangoFontDescriptionRef
	+ [x] pango\_layout\_set\_width
	+ [x] pango\_layout\_get\_width
	+ [ ] pango\_layout\_set\_height
	+ [ ] pango\_layout\_get\_height
	+ [ ] pango\_layout\_set\_wrap
	+ [ ] pango\_layout\_get\_wrap
	+ [ ] pango\_layout\_is\_wrapped
	+ [x] pango\_layout\_set\_ellipsize
	+ [ ] pango\_layout\_get\_ellipsize
	+ [ ] pango\_layout\_is\_ellipsized
	+ [x] pango\_layout\_set\_indent
	+ [ ] pango\_layout\_get\_indent
	+ no pango\_layout\_set\_spacing
	+ no pango\_layout\_get\_spacing
	+ [ ] wait until 1.44: pango\_layout\_set\_line\_spacing
	+ [ ] wait until 1.44: pango\_layout\_get\_line\_spacing
	+ [ ] pango\_layout\_set\_justify
	+ [ ] pango\_layout\_get\_justify
	+ [ ] pango\_layout\_set\_auto\_dir
	+ [ ] pango\_layout\_get\_auto\_dir
	+ [ ] pango\_layout\_get\_direction
	+ [x] pango\_layout\_set\_alignment
	+ [ ] pango\_layout\_get\_alignment
	+ [x] pango\_layout\_set\_tabs
	+ [ ] pango\_layout\_get\_tabs
	+ [x] pango\_layout\_set\_single\_paragraph\_mode
	+ [ ] pango\_layout\_get\_single\_paragraph\_mode
	+ [x] pango\_layout\_get\_unknown\_glyph\_count
	+ no pango\_layout\_get\_log\_attrs
	+ no pango\_layout\_get\_log\_attrs\_readonly
	+ [x] pango\_layout\_index\_to\_pos
		- [x] define PangoRectangle
	+ [x] pango\_layout\_index\_to\_line\_x
	+ [x] pango\_layout\_xy\_to\_index
	+ [x] pango\_layout\_get\_cursor\_pos
	+ [x] pango\_layout\_move\_cursor\_visually
	+ [x] pango\_layout\_get\_extents
	+ [x] pango\_layout\_get\_pixel\_extents
	+ [x] pango\_extents\_to\_pixels
	+ [x] pango\_layout\_get\_size
	+ [x] pango\_layout\_get\_pixel\_size
	+ [x] pango\_layout\_get\_baseline
	+ [x] pango\_layout\_get\_line\_count
	+ no pango\_layout\_get\_line
	+ [x] pango\_layout\_get\_line\_readonly
		- [x] define PangoLayoutLine: this is readonly
			* [x] whether or not to make PangoLayoutLine instance of Storable
	+ no pango\_layout\_get\_lines
	+ [x] pango\_layout\_get\_lines\_readonly
		- [x] define module to process GSList
	+ [x] pango\_layout\_get\_iter
		- [x] define PangoLayoutIter s
		- next define pango\_layout\_iter\_get\_run\_readonly
	+ [ ] pango\_layout\_iter\_copy
	+ [x] pango\_layout\_iter\_free
	+ [x] pango\_layout\_iter\_next\_run
	+ [x] pango\_layout\_iter\_next\_char
		- next pango\_layout\_iter\_get\_char\_extents
	+ [x] pango\_layout\_iter\_next\_cluster
	+ [x] pango\_layout\_iter\_next\_line
	+ [x] pango\_layout\_iter\_at\_last\_line
	+ [x] pango\_layout\_iter\_get\_index
	+ [x] pango\_layout\_iter\_get\_baseline
	+ no pango\_layout\_iter\_get\_run
	+ [x] pango\_layout\_iter\_get\_run\_readonly
		- [x] define PangoGlyphItem
		- [x] define type synonym PangoLayoutRun
		- next define pango\_cairo\_show\_glyph\_item
	+ no pango\_layout\_iter\_get\_line
	+ [x] pango\_layout\_iter\_get\_line\_readonly
		- next pango\_cairo\_show\_layout\_line
	+ [x] pango\_layout\_iter\_get\_layout
	+ [x] pango\_layout\_iter\_get\_char\_extents
	+ [x] pango\_layout\_iter\_get\_cluster\_extents
	+ [x] pango\_layout\_iter\_get\_run\_extents
	+ [x] pango\_layout\_iter\_get\_line\_yrange
	+ [x] pango\_layout\_iter\_get\_line\_extents
	+ [x] pango\_layout\_iter\_get\_layout\_extents
	+ [ ] pango\_layout\_line\_ref
	+ [ ] pango\_layout\_line\_unref
	+ [x] pango\_layout\_line\_get\_extents
	+ [x] pango\_layout\_line\_get\_pixel\_extents
	+ [x] pango\_layout\_line\_index\_to\_x
	+ [x] pango\_layout\_line\_x\_to\_index
	+ [x] pango\_layout\_line\_get\_x\_ranges
	+ [ ] wait until 1.44: pango\_layout\_line\_get\_height
* [x] define function of Tab Stops
	+ [x] pango\_tab\_array\_new
	+ no pango\_tab\_array\_new\_with\_potisions
	+ [x] pango\_tab\_array\_copy
		- use from freeze, thaw and so on
	+ [x] pango\_tab\_array\_free
	+ [x] pango\_tab\_array\_get\_size
	+ [x] pango\_tab\_array\_resize
	+ [x] pango\_tab\_array\_set\_tab
	+ [x] pango\_tab\_array\_get\_tab
	+ [x] pango\_tab\_array\_get\_tabs
	+ [x] pango\_tab\_array\_get\_positions\_in\_pixels
	+ [x] rename PangoTabArray to PangoTabArrayPrim
	+ [x] define new PangoTabArray
	+ [x] use new PangoTabArray
		- [x] pango\_tab\_array\_get\_size
		- [x] pango\_tab\_array\_get\_tab
		- [x] pango\_tab\_array\_get\_tabs
		- [x] pango\_tab\_array\_get\_positions\_in\_pixels
	+ [x] pangoTabArrayFreeze
	+ [x] pangoTabArrayThaw
* [ ] define functions of pango cairo
	+ no pango\_cairo\_font\_map\_get\_default
	+ ...
	+ no pango\_cairo\_font\_map\_create\_context
	+ no pango\_cairo\_font\_get\_scaled\_font
	+ [ ] pango\_cairo\_context\_set\_resolution
	+ [ ] pango\_cairo\_context\_get\_resolution
	+ no pango\_cairo\_context\_set\_font\_options
	+ no pango\_cairo\_context\_get\_font\_options
	+ no pango\_cairo\_context\_set\_shape\_renderer
	+ no pango\_cairo\_context\_get\_shape\_renderer
	+ [x] pango\_cairo\_create\_context
	+ [x] pango\_cairo\_update\_context
	+ [x] pango\_cairo\_create\_layout
	+ [x] pango\_cairo\_update\_layout
	+ no pango\_cairo\_show\_glyph\_string
	+ [x] pango\_cairo\_show\_glyph\_item
	+ [x] pango\_cairo\_show\_layout\_line
	+ [x] pango\_cairo\_show\_layout
	+ [x] pango\_cairo\_show\_error\_underline
	+ no pango\_cairo\_glyph\_string\_path
	+ [x] pango\_cairo\_layout\_line\_path
	+ [x] pango\_cairo\_layout\_path
	+ [x] pango\_cairo\_error\_underline\_path

### font description

* family
* style
* variant
* weight
* stretch
* size
* gravity
* variations

#### family

* sans, sans serif, sans-serif
* serif
* monospace
* cursive
* fantasy
* Liberation Sans
* LIberation Serif
* Liberation Mono
* Sazanami Mincho
* Sazanami Gothic
* etc

#### size

* thousandths of a point
* xx-small
* x-small
* small
* medium
* large
* x-large
* xx-large
* smaller
* larger

#### style

* normal
* italic
* oblique

#### weight

* ultralight
* light
* normal
* bold
* ultrabold
* heavy
* or numeric

#### variant

* normal
* smallcaps

#### stretch

* ultracondensed
* extracondensed
* condensed
* semicondensed
* normaal
* semiexpanded
* expanded
* extraexpanded
* ultraexpanded

#### gravity

#### variations

### packages

* simple-pango-font-description
* simple-pango-layout
* simple-pango-cairo
* simple-pango-types

### consider

* to separate System.Glib.SinglyLinkedLists as simple-glib-linked-lists with System.Glib.DoublyLinkedLists
