memo
====

todo
----

* [x] `newtype Foo`
* [x] `pattern Foo`
	+ [x] function foo
		- [x] for single field
		- [x] for multiple field
	+ [x] pattern signature
	+ [x] pattern body
		- [x] pattern match
		- [x] construct
* [x] `instance Show Foo`
* [x] `instance Read Foo`
	- [x] no template
	- [x] template
* [x] `newtype FooPrim`
	+ [x] repair
* [x] `function fooFreeze`
* [x] `function fooThaw`
* [x] `function fooCopy`
* [x] make derivings
	+ [x] Eq
		- [x] no template
		- [x] template
	+ [x] Ord
		- [x] no template
		- [x] template
	+ [x] Bounded
		- [x] no template
		- [x] template
	+ [x] Ix
		- [x] no template
		- [x] template
			* [x] `range`
			* [x] `index`
			* [x] `inRange`
			* [x] whole
* [x] add deriving argument
	+ [x] define type `Deriving`
	+ [x] others
* [x] `struct`
* [x] `structPrim`
	+ [x] change to `newtype FooPrim s = FooPrim (Ptr Foo)`
	+ [x] others
* [x] add `type FooIO = FooPrim RealWorld`
	+ [x] no template
	+ [x] template
	+ [x] add to `structPrim`
* [x] add `type FooST = FooPrim`
	+ [x] no template
	+ [x] template
	+ [x] add to `structPrim`
* [x] export only `struct` and `structPrim`
* [ ] refactoring
	+ [ ] Lib
		- [ ] header
		- [ ] import list
		- [ ] structure
		- [ ] body
	+ [ ] Template
		- [x] hlint
		- [ ] export list
		- [ ] import list
		- [ ] structure
		- [ ] body
* [ ] independence
* [ ] documentation
* [ ] publish
