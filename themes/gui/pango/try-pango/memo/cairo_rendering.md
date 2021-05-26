Cairo Rendering memo
====================

* [x] make `pangoCairoCreateContext` to return `PangoContext (PrimState m)`
* [x] make `pangoCairoCreateLayout` to use `PangoContext s` as argument
* [x] make `pangoCairoCreateLayout` to return `PangoLayoutPrim (PrimState m)`
* [ ] change from `PrimMonad m => m` to `IO`
	+ [x] `pangoCairoShowLayout`
	+ [ ] others

review
------

* [x] pangoCairoCreateContext
* [x] pangoCairoUpdateContext
* [x] pangoCairoCreateLayout
* [x] pangoCairoUpdateLayout
* [ ] pangoCairoShowGlyphItem
* [ ] pangoCairoShowLayoutLine
* [ ] pangoCairoShowLayout
* [ ] pangoCairoShowErrorUnderline
* [ ] pangoCairoLayoutLinePath
* [ ] pangoCairoLayoutPath
* [ ] pangoCairoErrorUnderlinePath
