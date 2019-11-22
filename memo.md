memo
====

definition of ($!)
------------------

### base 4.0.0.0 - 4.1.0.0

Prelude

```haskell
#ifndef __HUGS__
infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
#endif
```

### base 4.2.0.0 - 4.6.0.1

Prelude

```haskell
#ifndef __HUGS__
infixr 0 $!
#endif

($!) :: (a -> b) -> a -> b
#ifdef __GLASGOW_HASKELL__
f $! x = let !vx = x in f vx -- see #2273
#endif !defined(__HUGS__)
f $! x = x `seq` f x
#endif
```

### base 4.7.0.0 - 4.7.0.2

Prelude

```haskell
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx -- see #2273
```

### base 4.8.0.0 - 4.11.1.0

GHC.Base

```haskell
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx -- see #2273
```

### base 4.12.0.0

GHC.Base

```haskell
($!) :: forall r a (b :: TYPE r) . (a -> b) -> a -> b
f $! x = let !vx = x in f vx -- see #2273
```
