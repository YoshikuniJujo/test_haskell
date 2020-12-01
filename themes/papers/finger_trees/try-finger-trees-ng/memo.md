memo
====

* [ ] refactor about TypeCheck.Nat
	+ [ ] separate typecheck and expression judgement
	+ [ ] rename and organize modules
* [ ] consider whether or not to rename Range to Ranged

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
	+ [x] rename to doesContainVar
* [x] define containVars
* [ ] make module Given
	+ [x] make empty module
	+ [x] smart constructor remove equal Zero
	+ [x] define removeVar
		- [x] define removeVarInit
		- [x] define removeVarStep
		- [x] define removeVar
	+ [x] define removeVars
	+ [x] define `containVars :: Given v -> [v]`
	+ [x] define `expsToGiven :: [Exp v Bool] -> Given v`
		- use expToVarBool
		- [x] no Maybe
	+ [x] define `givenToZeros :: Given v -> [Zero v]`
	+ [ ] make export list
* [ ] make module Wanted
	+ [x] define `expToWanted :: Exp v Bool -> Wanted v`
	+ [x] define `wantedToZero :: Wanted v -> Zero v`
	+ [x] define `containVars :: Wanted v -> [v]`
	+ [ ] make export list
* [ ] make module Derive
	+ [x] define `canDerive :: Given v -> Wanted v -> Bool`
	+ [ ] make export list
* [x] make New.TypeCheck.Nat.solveNat
* [ ] make Annotated fingertree
	+ [x] make empty module
	+ [ ] others
* [x] remove Grt
	+ [x] use Geq (foo - bar - 1) instead of Grt (foo - bar) foo
	+ [x] remove Grt
* [x] no negative Nat
	+ [x] eqToZero: return list of Zero
	+ [x] termToPolynominal: return ([Zero v], Polynominal v)

refactor new TypeCheck
-----------------------

* [x] remove old typecheck
* [x] remove 'New' from module name hierarchy
* [x] try removing Polynominal
* [x] check module name hierarchy
* [ ] refactor module name hierarchy
	+ [x] add top level name Derivation
	+ [x] consider whether or not to rename TypeCheck.Nat to Plugin.TypeCheck.Nat.Simple
	+ [x] rename Polynominal.Type to Polynominal.Polynominal
	+ [x] remove middle name 'Polynominal' from module name
	+ [ ] add top name Foo to Derivation
	+ [ ] consider whether or not to rename Expression
	+ [ ] others

### module name hierarchy

```
Derivation
  + Expression
  + Polynominal
      + Derive
      + Given
      + Wanted
      + AvoidNegative
      + Zero
      + Polynominal
Plugin
  + TypeCheck
      + Nat
          + Simple
              + Decode
```
