Simple XML Subset Parser memo
=============================

* macro `G_MARKUP_ERROR`
* `enum GMarkupError`

todo
----

* [ ] `newtype GMarkupError`
	+ [ ] `pattern GMarkupErrorBadUtf8`
	+ [ ] `pattern GMarkupErrorEmpty`
	+ [ ] `pattern GMarkupErrorParse`
	+ [ ] `pattern GMarkupErrorUnknownElement`
	+ [ ] `pattern GMarkupErrorUnknownAttribute`
	+ [ ] `pattern GMarkupErrorInvalidContent`
	+ [ ] `pattern GMarkupErrorMissingAttribute`
* [ ] `pattern GErrorMarkup :: GMarkupError -> String -> GError`
