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
* [ ] make Data.Derivation.Test.Parse
	+ [x] make Internal.Tools.Parse
		- [x] until parse number
		- [x] rest
		- [x] not only Char but more general type as token
	+ [ ] make Data.Derivation.Parse
		- [x] define `term :: Parse String (Exp String Term)`
		- [x] define leq
		- [ ] define bool
			* [x] F, T, (<=)
			* [ ] (==)
		- [ ] define var
		- [ ] define eq
		- [ ] define `parseWanted :: String -> [Wanted String]`
		- [ ] define `parseGiven :: [String] -> [Given String]`
	+ [ ] make Data.Derivation.Parse.Packrat
		- [x] copy Data.Parse from try-packrat
		- [x] define expression parser
		- [x] define lesserEqual parser
		- [x] define variable parser
		- [x] define bool parser
			* [x] qualify Data.Bool
			* [x] except equal
			* [x] equal
		- [x] define equal parser
		- [x] define Wanted parser
		- [x] define Given parser
		- [x] define test data parser
* [x] make type WantedSet ADT
	+ [x] make `canDerive' :: Given -> WantedSet -> Bool`
	+ [x] use canDerive'
	+ [x] rename canDerive to canDerive or so
	+ [x] rename canDerive' to canDerive
	+ [x] make type WantedSet ADT
	+ [x] rename data type Wanted to Wanted1
	+ [x] rename data type WantedSet to Wanted
	+ [x] change to `data Wanted = Wanted [Zero]`
		- (Nothing, _) -> Nothing
		- (Just w, ws) -> Just (w : ws)
	+ [x] make ADT Wanted1 to type synonym

refactor parser of derivation
-----------------------------

* [x] check module name hierarchy
* [x] remove Data.Derivation.Parse
* [x] remove Internal.Tools.Parse
* [x] remove Internal.Tools.SampleParser
* [x] rename module Data.Derivation.Parse.Packrat to Data.Derivation.Parse
* [x] refactor module name hierarchy
	+ Data.Parse
	+ Data.Derivation.Parse
* [x] refactor Data.Parse
	+ [x] make export list
	+ [x] others
	+ [x] rename d to s
* [x] consider whether or not to rename Derivs and derivs
* [x] refactor Data.Derivation.Parse
	+ [x] make export list
	+ [x] add import list
	+ [x] structure
	+ [x] body
		- [x] define and use type synonym Var
		- [x] PARSE
		- [x] MEMO
			* [x] rename Derivs to Memo
			* [x] data Memo	
			* [x] function memo
			* [x] function check
			* [x] function pick
		- [x] GRAMMAR
			* [x] GIVEN WANTED, GIVEN AND WANTED
			* [x] CONSTRAINT
			* [x] POLYNOMIAL
		- [x] VAR
* [x] remove Trial.TypeCheck.ExpParser

### module name hierarchy

```
Data
  + Parse
  + Derivation
      + Parse
```

refactor new TypeCheck
-----------------------

* [x] remove old typecheck
* [x] remove 'New' from module name hierarchy
* [x] try removing Polynominal
* [x] check module name hierarchy
* [x] refactor module name hierarchy
	+ [x] add top level name Derivation
	+ [x] consider whether or not to rename TypeCheck.Nat to Plugin.TypeCheck.Nat.Simple
	+ [x] rename Polynominal.Type to Polynominal.Polynominal
	+ [x] remove middle name 'Polynominal' from module name
	+ [x] add top name Data to Derivation
	+ [x] rename module from Derive to CanDerive
	+ [x] consider whether or not to put module CanDerive, Given and Wanted in module CanDerive
	+ [x] consider whether or not to put module Zero and Polynominal in module Zero
	+ [x] consider whether or not to put module Expression and AvoidNegative in module AvoidNegative
	+ [x] rename module from AvoidNegative to Expression
	+ [x] rename Zero to Constraint
		- [x] rename module
		- [x] rename data type
* [x] refactor Data.Derivation.Constraint
	+ [x] remove debugFoo
	+ [x] export list
		- [x] rename identity
		- [x] others
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] CONSTRAINT
			* [x] DATA TYPE
			* [x] CONSTRUCTOR
				+ [x] equal
				+ [x] greatEqualThan
				+ [x] greatThan
				+ [x] regularizeEq
				+ [x] regularizeGeq
				+ [x] multiple
				+ [x] divide
				+ [x] divisor
			* [x] VARIABLE
				+ [x] getVars
				+ [x] hasVar
			* [x] REMOVE VAR
				+ [x] removeVar
				+ [x] alignEE
				+ [x] alignEG
				+ [x] alignGG
			* [x] REMOVE NEGATIVE, IS DERIVABLE AND SELF CONTAINED
				+ [x] removeNegative
				+ [x] isDerivableFrom
				+ [x] selfContained
		- [x] POLYNOMIAL
* [x] make data type Message
	+ [x] make Message Outputtable
	+ [x] use Message instead of Text
	+ [x] remove text from dependent package
* [x] refactor Data.Derivation.Expression
	+ [x] export list
		- [x] add export list
		- [x] refactor export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] DATA EXP
		- [x] MAKE CONSTRAINT
			* [x] makeConstraint
			* [x] procPred
				+ [x] Bool and Var
				+ [x] :<=
				+ [x] (_ :== Bool)
				+ [x] (Bool :== _)
				+ [x] (_ :== Var)
				+ [x] (Var :== _)
				+ [x] (_ :== _)
				+ [x] others
		- [x] MAKE POLYNOMIAL
		- [x] MAKE VAR BOOL
			* [x] type VarBool
			* [x] makeVarBool
			* [x] vbInit
			* [x] vbStep
			* [x] untilFixes
* [x] refactor Data.Derivation.CanDerive
	+ [x] export list
		- [x] add export list
		- [x] refactor export list
			* [x] rename expsToGiven to makeGiven
			* [x] rename expToWanted to makeWanted
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] CAN DERIVE
		- [x] REMOVE VARS
			* [x] rmVar
			* [x] rvStep
			* [x] rmVar1
			* [x] unfoldUntil
		- [x] GIVEN
		- [x] WANTED
* [ ] refactor Plugin.TypeCheck.Nat.Simple.Decode
	+ [x] export list
		- [x] add export list
		- [x] refactor export list
			* [x] rename decode'
	+ [x] import list
	+ [ ] structure
	+ [ ] body
* [ ] refactor Plugin.TypeCheck.Nat.Simple

### module name hierarchy

```
Data
  + Derivation
      + CanDerive
      + Expression
      + Constraint
Plugin
  + TypeCheck
      + Nat
          + Simple
              + Decode
```
