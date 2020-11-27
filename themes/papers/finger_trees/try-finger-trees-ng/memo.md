memo
====

* [ ] refactor about TypeCheck.Nat
	+ [ ] separate typecheck and expression judgement
	+ [ ] rename and organize modules

make new TypeCheck
------------------

* [x] try printing expression data type
* [x] make independent module New.Polynominal.Zero
	+ [x] make module New.Polynominal.Zero
	+ [x] define divisor
	+ [x] define divide
	+ [x] define multiple
	+ [x] define regularize
	+ [x] make smart constructor of Zero
	+ [x] export only smart constructor
	+ [x] use smart constructor
* [x] define removeVar
	+ `removeVar :: Zero v -> Zero v -> v -> [Zero v]`
* [x] define containVar
	+ [ ] rename to doesContainVar
* [ ] define containVars
* [ ] make module Given
	+ [x] make empty module
	+ [ ] others
* [ ] make module Derive
