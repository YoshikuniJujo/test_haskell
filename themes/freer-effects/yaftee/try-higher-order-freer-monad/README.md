# try-higher-order-freer

This package used by package yaftee.

```Haskell
type Eff effs = Data.HigherFunctor.H (Control.HigherOpenUnion.U effs)
```
