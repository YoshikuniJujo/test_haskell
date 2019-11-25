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

8 / 24

* ConstrClosure
* FunClosure
* ThunkClosure
* BlackholeClosure
* PAPClosure
* APClosure
* IntClosure
* FloatClosure

#### yet

* SelectorClosure
* APStackClosure
* IndClosure
* BCOClosure
* ArrWordsClosure
* MutArrClosure
* MVarClosure
* MutVarClosure
* BlockingQueueClosure
* WordClosure
* Int64Closure
* Word64Closure
* AddrClosure
* DoubleClosure
* OtherClosure
* UnsupportedClosure
