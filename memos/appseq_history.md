memo
====

definition of ($!)
------------------

### base 4.0.0.0 - 4.1.0.0

2009-02-10 - 2009-04-09

Prelude

```haskell
#ifndef __HUGS__
infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
#endif
```

### base 4.2.0.0 - 4.6.0.1

2009-12-15 - 2013-01-31

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

2014-04-08 - 2014-12-24

Prelude

```haskell
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx -- see #2273
```

### base 4.8.0.0 - 4.11.1.0

2015-03-27 - 2018-04-21

GHC.Base

```haskell
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx -- see #2273
```

### base 4.12.0.0

2018-09-23

GHC.Base

```haskell
($!) :: forall r a (b :: TYPE r) . (a -> b) -> a -> b
f $! x = let !vx = x in f vx -- see #2273
```
