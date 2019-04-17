メモ
====

newtype Endo a = Endo (a -> a)

instance Monoid (Endo a) where
	mempty = Endo id
	Endo f `mappend` Endo g = f . g
