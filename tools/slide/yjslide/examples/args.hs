{-# LANGUAGE FlexibleInstances, GADTs #-}

foo :: FooType a => a
foo = bar (return ())

-- default FooType (IO ())

class FooType a where
	bar :: IO () -> a

instance (a ~ ()) => FooType (IO a) where
	bar = id

instance (Show x, FooType r) => FooType (x -> r) where
	bar s x = bar (s >> print x)

class Hoge a where
	some :: a -> a

instance Hoge (IO a) where
	some = id

{-
instance (a ~ ()) => Hoge (IO a) where
	some = id
	-}
