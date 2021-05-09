memo
====

* [ ] define `pangoScriptForText :: Text -> [(Text, PangoScript)]`
	+ [x] `pango_script_iter_new`
	+ [x] `pango_script_iter_get_range`
	+ [x] `pango_script_iter_next`
	+ [x] `pango_script_iter_free`
	+ [ ] `withPangoScriptIter`
		- [x] define
		- [ ] use `bracket`
	+ [ ] define `pangoScriptForText`
