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
* [x] separate `Template.Parts` from `Template`
* [ ] refactoring
	+ [x] Lib
		- [x] header
		- [x] import list
		- [x] structure
		- [x] body
			* [x] DEFINITION
			* [x] SAMPLE
			* [x] INSTANCE IX CINT
	+ [ ] Template
		- [x] hlint
		- [x] export list
		- [x] import list
		- [x] structure
		- [ ] body
			* [ ] STRUCT
				+ [x] FUNCTION STRUCT
					- [x] define type for document
					- [x] others
				+ [ ] NEWTYPE
				+ [ ] PATTERN
					- [ ] Function Mk Pattern
					- [ ] Function Mk Pattern Fun
				+ [ ] DERIVING
					- [ ] Function Mk Deriving
					- [ ] Show
					- [ ] Read
					- [ ] Eq
					- [ ] Ord
					- [ ] Bounded
					- [ ] Ix
			* [ ] STRUCT WITH PRIMITIVE MONAD
				+ [ ] FUNCTION STRUCT PRIM
				+ [ ] NEWTYPE
				+ [ ] FREEZE
				+ [ ] THAW
				+ [ ] COPY
	+ [ ] Template.Parts
* [ ] independence
* [ ] documentation
* [ ] publish
