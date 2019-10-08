{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=GHC.TypeLits.Normalise #-}

module NList where

import GHC.TypeLits

infixr 6 :.

data NList :: Nat -> * -> * where
	Empty :: NList 0 a
	(:.) :: a -> NList n a -> NList (n + 1) a

deriving instance Show a => Show (NList n a)

infixr 5 ++.

(++.) :: NList n a -> NList m a -> NList (n + m) a
Empty ++. ys = ys
(x :. xs) ++. ys = x :. (xs ++. ys)

reverse' :: NList n a -> NList m a -> NList (n + m) a
reverse' r Empty = r
reverse' r (x :. xs) = reverse' (x :. r) xs
