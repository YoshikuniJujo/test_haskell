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

14 / 24

* ConstrClosure
* FunClosure
* ThunkClosure
* BlackholeClosure
* PAPClosure
* APClosure
* IntClosure
* FloatClosure
* DoubleClosure
* WordClosure
* AddrClosure
* MutVarClosure
* MVarClosure
* ArrWordsClosure

#### nexts


#### yet

* SelectorClosure
* APStackClosure
* IndClosure
* BCOClosure
* BlockingQueueClosure

#### later

* Int64Closure
* Word64Closure
* MutArrClosure
* OtherClosure
* UnsupportedClosure
