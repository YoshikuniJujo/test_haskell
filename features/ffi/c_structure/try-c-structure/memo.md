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
	+ [x] Template
		- [x] hlint
		- [x] export list
		- [x] import list
		- [x] structure
		- [x] body
			* [x] STRUCT
				+ [x] FUNCTION STRUCT
					- [x] define type for document
					- [x] others
				+ [x] NEWTYPE
				+ [x] PATTERN
					- [x] Function Mk Pattern
						* [x] function `mkPatternSig`
						* [x] function `mkPatternBody`
						* [x] function `mkPatternBodyClause`
					- [x] Function Mk Pattern Fun
						* [x] function `mkPatternFunSig`
						* [x] function `mkPatternFunBody`
						* [x] function `mkPatternFunDo`
				+ [x] DERIVING
					- [x] Function Mk Deriving
						* [x] function `mkInstances`
						* [x] `data Deriving`
						* [x] function `toDeriving`
						* [x] `pattern NameFoo`
					- [x] Show
						* [x] function `mkInstanceShow`
						* [x] function `mkShowFields`
					- [x] Read
						* [x] function `mkInstanceRead`
						* [x] function `mkReadMems`
					- [x] Eq
						* [x] function `mkInstanceEq`
						* [x] function `fieldEqual`
					- [x] Ord
						* [x] function `mkInstanceOrd`
						* [x] function `lamOrd`
					- [x] Bounded
					- [x] Ix
						* [x] mkInstanceIx
						* [x] mkRange
						* [x] mkIndex
						* [x] mkIndexLam
						* [x] mkInRange
			* [x] STRUCT WITH PRIMITIVE MONAD
				+ [x] FUNCTION STRUCT PRIM
				+ [x] NEWTYPE AND TYPE SYNONYM
					- [x] mkNewtypePrim
					- [x] mkTypeIO
					- [x] mkTypeST
				+ [x] FREEZE
					- [x] mkFreezeSig
					- [x] mkFreezeFun
					- [x] mkFreezeBody
				+ [x] THAW
					- [x] mkThawSig
					- [x] mkThawFun
					- [x] mkThawBody
				+ [x] COPY
					- [x] mkCopySig
					- [x] mkCopyFun
					- [x] mkCopyBody
	+ [ ] Template.Parts
		- [x] export list
		- [x] import list
		- [ ] structure
		- [ ] body
* [x] change from `bar :: Bar -> Unit CInt` to `bar :: Bar -> CInt`
* [x] rename from `litI` to `intE`
* [ ] independence
* [ ] documentation
* [ ] publish
