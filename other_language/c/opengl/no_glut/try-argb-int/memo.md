memo
====

* [ ] exception if error to create context
	+ [ ] make execution for error
	+ [ ] glXChooseVisual
		- [ ] process `NULL`
	+ [ ] glXCreateContext
		- [ ] process `NULL`
		- [ ] check errors
			* [ ] `BadMatch`
			* [ ] `BadValue`
			* [ ] `GLXBadContext`
			* [ ] `BadAlloc`
		- [ ] make exceptions
		- [ ] throw exceptions
	+ [ ] glXMakeCurrent
* [x] make function `foo :: IO () -> IO Image`
* [x] use `ForeignPtr`
* [x] return `cairo-image.Data.CairoImage.Internal.Argb32`
