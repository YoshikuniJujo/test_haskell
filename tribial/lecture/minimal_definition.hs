class Foo a where
	{-# MINIMAL foo | bar #-}
	foo :: a
	bar :: a

	foo = bar
	bar = foo

instance Foo Int where
	foo = 12345

instance Foo Char where
