memo
====

* [ ] define `pangoScriptForText :: Text -> [(Text, PangoScript)]`
	+ [x] `pango_script_iter_new`
	+ [x] `pango_script_iter_get_range`
	+ [x] `pango_script_iter_next`
	+ [x] `pango_script_iter_free`
	+ [x] `withPangoScriptIter`
		- [x] define
		- [x] use `bracket`
	+ [x] define `pangoScriptIterGetRange`
	+ [x] define `pangoScriptIterNext`
	+ [x] define `nullableCStringPart`
	+ [x] define `pangoScriptIterGetRanges`
	+ [ ] define `pangoScriptForText`
		- [x] define
		- [ ] use unsafeInterleaveIO
