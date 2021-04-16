PangoFontDescription memo
=========================

functions
---------

* `pango_font_description_new`
* `pango_font_description_copy`
* `pango_font_description_copy_static`
* `pango_font_description_hash`
* `pango_font_description_equal`
* `pango_font_description_free`
* `pango_font_descriptions_free`
* `pango_font_description_set_family`
* `pango_font_description_set_family_static`
* `pango_font_description_get_family`
* `pango_font_description_set_style`
* `pango_font_description_get_style`
* `pango_font_description_set_variant`
* `pango_font_description_get_variant`
* `pango_font_description_set_weight`
* `pango_font_description_get_weight`
* `pango_font_description_set_stretch`
* `pango_font_description_get_stretch`
* `pango_font_description_set_size`
* `pango_font_description_set_absolute_size`
* `pango_font_description_get_size_is_absolute`
* `pango_font_description_set_gravity`
* `pango_font_description_get_gravity`
* `pango_font_description_set_variations`
* `pango_font_description_set_variations_static`
* `pango_font_description_get_variations`
* `pango_font_description_get_set_fiedlds`
* `pango_font_description_unset_fields`
* `pango_font_description_merge`
* `pango_font_description_merge_static`
* `pango_font_description_better_match`
* `pango_font_description_from_string`
* `pango_font_description_to_string`
* `pango_font_description_to_filename`

todo
----

* [x] use `PangoDescriptionPrim (PrimState m)`
	+ [x] comment out `pangoFontDescriptionEqual`
	+ [x] `pangoFontDescriptionGetFamily`
	+ [x] `pangoFontDescriptionGetStyle`
	+ [x] `pangoFontDescriptionGetVariant`
	+ [x] `pangoFontDescriptionGetWeight`
	+ [x] `pangoFontDescriptionGetStretch`
	+ [x] `pangoFontDescriptionGetSize`
	+ [x] `pangoFontDescriptionGetSizeIsAbsolute`
	+ [x] `pangoFontDescriptionGetGravity`
	+ [x] `pangoFontDescriptionGetSetField`
	+ [x] `pangoFontDescriptionToString`
	+ [x] `pangoFontDescriptionToFilename`
* [x] remove `PangoDescription`
* [x] try to remove `PangoContextOld`
* [x] rename `PangoDescriptionPrim (PrimState m)` to `PangoDescription (PrimState m)`
* [x] use `unsafeIOToPrim` instead of `unprimIo`
	+ [x] `pangoFontDescriptionNew`
	+ [x] `pangoFontDescriptionCopy`
	+ [x] others
* [x] remove `unprimIo`
* [x] `pango_font_description_new`
* [x] move the description of the ADT `PangoFontDescription` to `Graphics.Pango.Basic.Fonts`
* [x] `pango_font_description_free`
* [x] define a type class `PangoFontDescriptionSetting`
	+ [x] change to `pangoFontDescriptionGetUnsafe`
	+ [x] add `pangoFontDescriptionMaskBit`
* [x] define `pangoFontDescriptionGet`
* [x] `pango_font_description_get_set_fields`
* [x] define `pangoFontDescriptionUnset`
* [x] `pango_font_description_set_foo and pango_font_description_get_foo`
	+ [x] family
		- [x] pangoFontDescriptionGet: check `set_fields`
	+ [x] style
	+ [x] variant
	+ [x] weight
	+ [x] stretch
	+ [x] size
	+ [x] gravity
* [x] make a module `Graphics.Pango.Basic.Fonts.PangoFontDescription`
* [x] make a module `Graphics.Pango.Basic.Fonts.PangoFontDescription.Type`
* [x] make a module `Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations`
* [x] `pango_font_description_set_variations` and `pango_font_description_get_variations`
	+ [x] define `readVariation :: BS.ByteString -> Map BS.ByteString Double`
	+ [x] define `showVariation :: Map BS.ByteString Double -> BS.ByteString`
	+ [x] define `pangoFontDescriptionSetVariationsMap`
		- `PangoFontDescription (PrimState m) -> Map BS.ByteString Double -> m ()`
	+ [x] define `c_pango_font_description_get_variations`
	+ [x] define `pangoFontDescriptionGetVariationsMap`
		- `pangoFontDescription (PrimState m) -> m (Map BS.ByteString Double)`
	+ [x] define `class PangoFontDescriptionAxis a`
		- pangoFontDescriptionSetAxis
		- pangoFontDescriptionGetAxis
	+ [x] define axes
		- [x] `Weight`
		- [x] `Width`
		- [x] `Italic`
		- [x] `Slant`
		- [x] `OpticalSize`
	+ [x] define template `pangoFontDescriptionAddAxis`
	+ [x] test customs
* [x] `pango_font_description_merge`
* [x] `pango_font_description_better_match`
* [x] `pango_font_description_to_string`
* [x] `pango_font_description_to_filename`
* [x] rename `PangoFontDescription` to `PangoFontDescriptionPrim`
* [x] define `PangoFontDescription`
* [ ] define `pangoFontDescriptionFreeze`
* [ ] define `pangoFontDescriptionThaw`

not todo yet
------------

* `pango_font_description_copy`
* `pango_font_description_equal`
* `pango_font_description_copy_static`
* `pango_font_description_hash`
* `pango_font_descriptions_free`
* `pango_font_description_set_family_static`
* `pango_font_description_set_variations_static`
* `pango_font_description_merge_static`
* `pango_font_description_from_string`

five registered axes
--------------------

* Weight (wght): 100 is thin, 400 is extra bold
* Width (wdth): 100 is normal width, 200 is double-width, 50 is half-width
* Italic (ital): 0 means roman letters and 1 means italic forms
* Slant (slnt): value sets the slant angle in degrees (from -90 to 90)
* Optical Size (opsz): a point size that the design can respond to

Decovar font axes
-----------------

| tag  | name                 | default | min | max  |
|------|----------------------|---------|-----|------|
| BLDA | Inline               | 0       | 0   | 1000 |
| BLDB | Worm                 | 0       | 0   | 1000 |
| WMX2 | Weight               | 0       | 0   | 1000 |
| SKLA | Inline Skeleton      | 0       | 0   | 1000 |
| SKLB | Worm Skeleton        | 0       | 0   | 1000 |
| SKLD | Stripes              | 0       | 0   | 1000 |
| TRMA | Rounded              | 0       | 0   | 1000 |
| TRMB | Flared               | 0       | 0   | 1000 |
| TRMC | Rounded Slab         | 0       | 0   | 1000 |
| TRMD | Sheared              | 0       | 0   | 1000 |
| TRME | Bifurcated           | 0       | 0   | 1000 |
| TRMF | Open Inline Terminal | 0       | 0   | 1000 |
| TRMG | Slab                 | 0       | 0   | 1000 |
| TRMK | Inline Terminal      | 0       | 0   | 1000 |
| TRML | Worm Terminal        | 0       | 0   | 1000 |
