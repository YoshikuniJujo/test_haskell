memo
====

```haskell
f >=> g = \x -> f x >>= g

(f >=> g) >=> h
==> (\x -> f x >>= g) >=> h
==> \y -> (\x -> f x >>= g) y >>= h
==> \y -> (f y >>= g) >>= h

f >=> (g >=> h)
==> f >=> (\x -> g x >>= h)
==> \y -> f y >>= (\x -> g x >>= h)

((f >=> g) >=> h) >=> i
==> ((\x -> f x >>= g) >=> h) >=> i
==> (\y -> (\x -> f x >>= g) y >>= h) >=> i
==> \z -> (\y -> (\x -> f x >>= g) y >>= h) z >>= i
==> \z -> f z >>= (g >>= (h >>= i))

f >=> (g >=> (h >=> i))
==> \z -> f z >>= (\y -> g y >>= (\x -> i x))
```

```haskell
f >=> g
==> \x -> f x >>= g

f x >>= g
Pure y >>= g
g y

f x >>= g
Bind tx k >>= g
Bind tx (k >=> f)
Bind tx (\y -> k y >>= f)
```

```haskell
(f >=> g) >=> h
==> \y -> f y >>= g >>= h

(f y >>= g) >>= h
(Bind ty k >>= g) >>= h
==> (Bind ty (k >=> g)) >>= h
==> Bind ty ((k >=> g) >=> h)

f >=> (g >=> h)
==> \y -> f y >>= (\x -> g x >>= h)

f y >>= (\x -> g x >>= h)
==> Bind ty k >>= (\x -> g x >>= h)
==> Bind ty (k >=> (\x -> g x >>= h))
```

```haskell
f = const $ Bind tx k

(f >=> f) >=> f $ z
\x -> (f >=> f) x >>= f $ z
(f >=> f) z >>= f
(f z >>= f) >>= f
(Bind tx k >>= f) >>= f
Bind tx (k >=> f) >>= f
Bind tx ((k >=> f) >=> f)

tx = Writer "foo"
k = Pure

runWriter $ Bind tx ((k >=> f) >=> f)
==> second ("foo" <>) <$> runWriter (((Pure >=> f) >=> f) ())
==> second ("foo" <>) <$> runWriter ((Pure >=> f) () >>= f)
==> second ("foo" <>) <$> runWriter ((Pure () >>= f) >>= f)
==> second ("foo" <>) <$> runWriter (f () >>= f)
==> second ("foo" <>) <$> runWriter (Bind tx k >>= f)
==> second ("foo" <>) <$> runWriter (Bind tx (k >=> f))
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Pure >=> f) ()
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Pure () >>= f)
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (f ())
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Bind tx k)
==> second ("foo" <>) <$> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (k ())
==> second ("foo" <>) <$> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Pure ())
==> second ("foo" <>) <$> second ("foo" <>) <$> second ("foo" <>) <$> Pure ((), mempty)
==> Pure ((), "foofoofoo")

f >=> (f >=> f) $ z
f z >>= (f >=> f)
Bind tx k >>= (f >=> f)
Bind tx (k >=> (f >=> f))

runWriter $ Bind tx (k >=> (f >=> f))
==> second ("foo" <>) <$> runWriter ((Pure >=> (f >=> f)) ())
==> second ("foo" <>) <$> runWriter (Pure () >>= (f >=> f))
==> second ("foo" <>) <$> runWriter ((f >=> f) ())
==> second ("foo" <>) <$> runWriter (f () >>= f)
==> second ("foo" <>) <$> runWriter (Bind tx k >>= f)
==> second ("foo" <>) <$> runWriter (Bind tx (k >=> f))
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter ((k >=> f) ())
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (k () >>= f)
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Pure () >>= f)
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (f ())
==> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Bind tx k)
==> second ("foo" <>) <$> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (k ())
==> second ("foo" <>) <$> second ("foo" <>) <$> second ("foo" <>) <$> runWriter (Pure ())
==> second ("foo" <>) <$> second ("foo" <>) <$> second ("foo" <>) <$> Pure ((), mempty)
==> Pure ((), "foofoofoo")
```

```haskell
((f >=> f) >=> f) >=> f $ ()
==> ((f >=> f) >=> f) () >>= f
==> (((f >=> f) ()) >>= f) >>= f
==> ((f () >>= f) >>= f) >>= f
==> ((Bind tx Pure >>= f) >>= f) >>= f
==> (Bind tx (Pure >=> f) >>= f) >>= f
==> Bind tx ((Pure >=> f) >=> f) >>= f
==> Bind tx (((Pure >=> f) >=> f) >=> f)

f >=> (f >=> (f >=> f)) $ ()
==> f () >>= (f >=> (f >=> f))
==> Bind tx Pure >>= (f >=> (f >=> f))
==> Bind tx (Pure >=> (f >=> (f >=> f)))
```

State effect
------------

```haskell
f = Bind u k

runFoo $ (f >> f) >> f
==> runFoo $ (f >>= const f) >>= const f
==> runFoo $ (Bind u k >>= const f) >>= const f
==> runFoo $ Bind u (k >=> const f) >>= const f

==> runFoo $ Bind u ((k >=> const f) >=> const f)
==> runFoo $ ((k >=> const f) >=> const f) a
==> runFoo $ (k >=> const f) a >>= const f
==> runFoo $ (k a >>= const f) >>= const f
==> runFoo $ (Pure a >>= const f) >>= const f
==> runFoo $ const f a >>= const f
==> runFoo $ f >>= const f
==> runFoo $ f >> f

runFoo $ f >> (f >> f)
==> runFoo $ f >>= const (f >> f)
==> runFoo $ Bind u k >>= const (f >> f)

==> runFoo $ Bind u (k >=> const (f >> f))
==> runFoo $ (k >=> const (f >> f)) a
==> runFoo $ k a >>= const (f >> f)
==> runFoo $ Pure a >>= const (f >> f)
==> runFoo $ const (f >> f) a
==> runFoo $ f >> f
```

```haskell
runFoo $ ((f >> f) >> f) >> f
==> runFoo $ ((f >>= const f) >>= const f) >>= const f
==> runFoo $ ((Bind u k >>= const f) >>= const f) >>= const f
==> runFoo $ (Bind u (k >=> const f) >>= const f) >>= const f
==> runFoo $ Bind u ((k >=> const f) >=> const f) >>= const f

==> runFoo $ Bind u (((k >=> const f) >=> const f) >=> const f)
==> runFoo $ (((k >=> const f) >=> const f) >=> const f) a
==> runFoo $ ((k >=> const f) >=> const f) a >>= cosnt f
==> runFoo $ ((k >=> const f) a >>= const f) >>= const f
==> runFoo $ ((k a >>= const f) >>= const f) >>= const f
==> runFoo $ ((Pure a >>= const f) >>= const f) >>= const f
==> runFoo $ (const f a >>= const f) >>= const f
==> runFoo $ (f >>= const f) >>= const f

runFoo $ f >> (f >> (f >> f))
==> runFoo $ f >>= const (f >> (f >> f))
==> runFoo $ Bind u k >>= const (f >> (f >> f))

==> runFoo $ Bind u (k >=> const (f >> (f >> f)))
==> runFoo $ (k >=> const (f >> (f >> f))) a
==> runFoo $ k a >>= const (f >> (f >> f))
==> runFoo $ Pure a >>= const (f >> (f >> f))
==> runFoo $ const (f >> (f >> f)) a
==> runFoo $ f >> (f >> f)
```

```haskell
f x = Bind u k

runFoo $ Bind u ((f >=> f) >=> f)
==> runFoo $ ((f >=> f) >=> f) a
==> runFoo $ (f >=> f) a >>= f
==> runFoo $ (f a >>= f) >>= f
==> runFoo $ (Bind u k >>= f) >>= f
==> runFoo $ Bind u (k >=> f) >>= f
==> runFoo $ Bind u ((k >=> f) >=> f)

runFoo $ Bind u (f >=> (f >=> f))
==> runFoo $ (f >=> (f >=> f) a
==> runFoo $ f a >>= (f >=> f)
==> runFoo $ Bind u k >>= (f >=> f)
==> runFoo $ Bind u (k >=> (f >=> f))
```

```haskell
f x = Bind u k

((f >=> f) >=> f) x
==> (f >=> f) x >>= f
==> (f x >>= f) >>= f
==> (Bind u k >>= f) >>= f
==> Bind u (k >=> f) >>= f
==> Bind u ((k >=> f) >=> f)

(f >=> (f >=> f)) x
==> f x >>= (f >=> f)
==> Bind u k >>= (f >=> f)
==> Bind u (k >=> (f >=> f))

fl(n + 1) x = (fl(n) >=> f) x
==> fl(n) x >>= f
==> (fl(n - 1) x >>= f) >>= f
...
==> ((...((f x >>= f) >>= f) ...) >>= f) >>= f
==> ((...((Bind u k >>= f) >>= f) ...) >>= f) >>= f
==> ((...(Bind u (k >=> f) >>= f) ...) >>= f) >>= f
==> ((... Bind u ((k >=> f) >=> f) ...) >>= f) >>= f
...
==> Bind u (((...(k >=> f) >=> f) ...) >>= f)

fr (n + 1) x = (f >=> fr(n)) x
==> f x >>= fr(n)
==> Bind u k >>= fr(n)
==> Bind u (k >=> fr(n))
```

```
f >=> f == \x -> f x >>= f

(\x -> f x >>= f) >=> f == \y -> (\x -> f x >>= f) y >>= f == \y -> (f y >>= f) >>= f
```

```
tx `Bind` k >>= f = tx `Bind` (k >=> f)
==> tx `Bind` (\x -> k x >>= f)

f x = tx `Bind` k

(f x >>= f) >>= f
==> (tx `Bind` k >>= f) >>= f
==> tx `Bind` (k >=> f) >>= f
==> tx `Bind` ((k >=> f) >=> f)
```

Codensity Monad
---------------

```haskell
abs $ (rep m >>= rep . f) >>= rep . g
==> ((m >>=) >>= (>>=) . f) >>= (>>=) . g $ return
==> (\k -> (m >>=) (\x -> f x >>= k)) >>= (>>=) . g $ return
==> (\k' -> (\k -> (m >>=) (\x -> f x >>= k)) (\y -> g y >>= k')) return
==> (\k -> (m >>=) (\x -> f x >>= k)) (\y -> g y >>= return)
==> (\k -> (m >>=) (\x -> f x >>= k)) (\y -> g y)
==> (\k -> (m >>=) (\x -> f x >>= k)) g
==> (m >>=) (\x -> f x >>= g)
==> m >>= \x -> f x >>= g
```

```haskell
rep m >>= rep . f
==> (m >>=) >>= (>>=) . f
==> \k -> m >>= (\x -> f x >>= k)
```

```haskell
m >>= f = \k -> m (\x -> f x k)
```

```haskell
m >>= (\x -> f x >>= k)

k <-- \x -> g x >>= k'
```

```haskell
rep m >>= rep . f
==> (m >>=) >>= (>>=) . f
==> \k -> m >>= \x -> f x >>= k
```

```haskell
(\k -> m >>= \x -> f x >>= k) >>= (>>=) . g
==> \k' -> m >>= \x -> f x >>= \y -> g y >>= k'
```

```haskell
\x -> ((>>=) . f) x k
\x -> f x >>= k
```

```haskell
m >>= rep . f
==> m >>= (>>=) . f
==> \k -> m (\x -> f x >>= k)
```
