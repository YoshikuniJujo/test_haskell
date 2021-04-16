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
* [ ] use `IO` instead of `PrimMonad m => m`
* [ ] change from `PangoLayoutPrim s` to `PangoLayoutPrim`
* [ ] remove `PangoLayout`
* [ ] rename `PangoLayoutPrim` to `PangoLayout`
