{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryTypeFamilies where

class FromThree d a b c where
	fromThree :: a -> b -> c -> d

instance {-# INCOHERENT #-} FromThree a a b c where
	fromThree x _ _ = x

instance {-# INCOHERENT #-} FromThree b a b c where
	fromThree _ y _ = y

instance {-# INCOHERENT #-} FromThree c a b c where
	fromThree _ _ z = z

foo :: (FromThree Char a b c, FromThree Integer a b c, FromThree Double a b c) =>
	(a, b, c) -> (Char, Integer, Double)
foo (x, y, z) = (fromThree x y z, fromThree x y z, fromThree x y z)

bar :: (FromThree t1 a b c, FromThree t2 a b c, FromThree t3 a b c) =>
	(a, b, c) -> (t1, t2, t3)
bar (x, y, z) = (fromThree x y z, fromThree x y z, fromThree x y z)
