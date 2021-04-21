Simple XML Subset Parser memo
=============================

* macro `G_MARKUP_ERROR`
* `enum GMarkupError`

todo
----

* [x] `newtype GMarkupError`
	+ [x] `pattern GMarkupErrorBadUtf8`
	+ [x] `pattern GMarkupErrorEmpty`
	+ [x] `pattern GMarkupErrorParse`
	+ [x] `pattern GMarkupErrorUnknownElement`
	+ [x] `pattern GMarkupErrorUnknownAttribute`
	+ [x] `pattern GMarkupErrorInvalidContent`
	+ [x] `pattern GMarkupErrorMissingAttribute`
* [x] `pattern GErrorMarkup :: GMarkupError -> String -> GError`
