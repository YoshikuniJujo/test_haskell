Layout Objects memo
===================

todo
----

* [x] make module `Graphics.Pango.Basic.LayoutObjects.PangoLayoutIo`
* [x] move `PangoLayoutIo` to it
* [x] make module `Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim`
* [x] define `PangoLayoutPrim s`
* [x] make `pangoLayoutNew` to return `PangoLayoutPrim (PrimState m)`
* [x] make `pangoLayoutSetFontDescription` to use `PangoFontDescriptionPrim (PrimState m)` as argument
* [x] remove `PangoFontDescription`
* [x] rename `PangoFontDescriptionPrim (PrimState m)` to `PangoFontDescription (PrimState m)`
* [x] use `IO` instead of `PrimMonad m => m`
	+ [x] pangoLayoutSetIndent
	+ [x] pangoLayoutSetAlignment
	+ [x] pangoLayoutSetTabs
	+ [x] pangoLayoutSetSingleParagraphMode
* [x] change from `PangoLayoutPrim s` to `PangoLayoutPrim`
* [x] remove `PangoLayout`
* [x] rename `PangoLayoutPrim` to `PangoLayout`
* [x] use `IO` instead of pure: pangoLayoutGetFoo
* [ ] repair about `PangoLayoutIter`
