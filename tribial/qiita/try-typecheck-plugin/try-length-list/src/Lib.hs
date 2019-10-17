{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import GHC.TypeLits

infixr 5 :.

data List :: Nat -> * -> * where
	Nil :: List 0 a
	(:.) :: a -> List ln a -> List (ln + 1) a

deriving instance Show a => Show (List ln a)

-- tail_ :: List (n + 1) a -> List n a
-- tail_ (_ :. xs) = xs
