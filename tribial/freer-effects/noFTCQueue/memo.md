memo
====

data FFree f a = forall b . FMap (b -> a) (f b)

instance Functor (FFree f) where
	fmap f (FMap g a) = FMap (f . g) a

data Free f a
	= Pure a
	| Join (f (Free f a))

type Freer f = Free (FFree f)

---

data Free t a = Pure a | Join (t (Free t a))

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)

type Freer = Free (Coyoneda t) a

Free (Coyoneda t) a
	= Pure a
	| Join (Coyoneda t (Free (Coyoneda t) a))

Free (Coyoneda t) a
	= Pure a
	| forall x . Join (Coyoneda (t x) (x -> Free (Coyoneda t) a))

type Coyoneda t = forall x . (t x, x -> a)

Free (Coyoneda t) a
	= Pure a
	| Join (Coyoneda t (Free (Coyonead t) a))

Free (Coyoneda t) a
	= Pure a
	| forall x . Join (t x, x -> Free (Coyoneda t) a)

type Freer = Free (Coyoneda t)

Freer t a
	= Pure a
	| forall x . Join (t x) (x -> Freer t a)

---

data Free t a
	= Pure a
	| Join (t (Free t a))

type Coyoneda t = forall x . (t x, x -> a)

type Freer t = Free (Coyoneda t)

Freer t a
	= Pure a
	| Join (Coyoneda t (Freer t a))

Freer t a
	= Pure a
	| forall x . Join (t x, x -> Freer t a)

data Freer t a
	= Pure a
	| forall x . Join (t x) (x -> Freer t a)
