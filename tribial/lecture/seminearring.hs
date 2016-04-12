{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid
import Test.QuickCheck

prop_monoid_unit :: (Monoid a, Eq a) => a -> Bool
prop_monoid_unit x = mempty `mappend` x == x && x `mappend` mempty == x

prop_monoid_commut :: (Monoid a, Eq a) => a -> a -> a -> Bool
prop_monoid_commut x y z =
	(x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)

class Monoid a => Seminearring a where
	one :: a
	mul :: a -> a -> a

prop_seminearring_unit :: (Seminearring a, Eq a) => a -> Bool
prop_seminearring_unit x = one `mul` x == x && x `mul` one == x

prop_seminearring_distl :: (Seminearring a, Eq a) => a -> a -> a -> Bool
prop_seminearring_distl x y z =
	(x `mappend` y) `mul` z ==
	(x `mul` z) `mappend` (y `mul` z)

prop_seminearring_distr :: (Seminearring a, Eq a) => a -> a -> a -> Bool
prop_seminearring_distr x y z =
	x `mul` (y `mappend` z) ==
	(x `mul` y) `mappend` (x `mul` z)

prop_seminearring_zero :: (Seminearring a, Eq a) => a -> Bool
prop_seminearring_zero x = mempty `mul` x == mempty && x `mul` mempty == mempty

newtype LL a = LL { unLL :: [[a]] } deriving (Eq, Show, Monoid, Arbitrary)

instance Seminearring (LL a) where
	one = LL [[]]
	LL xss `mul` LL yss = LL [ xs ++ ys | xs <- xss, ys <- yss ]
