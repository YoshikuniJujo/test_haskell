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

```haskell

```
