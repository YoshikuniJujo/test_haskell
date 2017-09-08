memo
====

data FFree f a = forall b . FMap (b -> a) (f b)

instance Functor (FFree f) where
	fmap f (FMap g a) = FMap (f . g) a

data Free f a
	= Pure a
	| Join (f (Free f a))

type Freer f = Free (FFree f)
