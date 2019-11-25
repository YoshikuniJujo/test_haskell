memo
====

Closure
-------

```haskell
type Closure = GenClosure Box

data GenClosure b = ...
```

### GenClosure

* ConstrClosure
* FunClosure
* ThunkClosure
* SelectorClosure
* PAPClosure
* APClosure
* APStackClosure
* IndClosure
* BCOClosure
* BlackholeClosure
* ArrWordsClosure
* MutArrClosure
* MVarClosure
* MutVarClosure
* BlockingQueueClosure
* IntClosure
* WordClosure
* Int64Closure
* Word64Closure
* AddrClosure
* FloatClosure
* DoubleClosure
* OtherClosure
* UnsupportedClosure

#### already

12 / 24

* ConstrClosure
* FunClosure
* ThunkClosure
* BlackholeClosure
* PAPClosure
* APClosure
* IntClosure
* FloatClosure
* WordClosure
* AddrClosure
* MutVarClosure
* MVarClosure

#### nexts

* ArrWordsClosure
* MutArrClosure

#### yet

* SelectorClosure
* APStackClosure
* IndClosure
* BCOClosure
* BlockingQueueClosure
* Int64Closure
* Word64Closure
* DoubleClosure
* OtherClosure
* UnsupportedClosure
