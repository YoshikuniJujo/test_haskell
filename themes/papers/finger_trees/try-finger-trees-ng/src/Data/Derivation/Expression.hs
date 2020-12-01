{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression where

import Outputable

-- 123, x, True, (123 *), (+), (-), (<=), (==),

data Term

data Exp v t where
	Bool :: Bool -> Exp v Bool
	Const :: Integer -> Exp v Term
	Var :: v -> Exp v a
	(:+) :: Exp v Term -> Exp v Term -> Exp v Term
	(:-) :: Exp v Term -> Exp v Term -> Exp v Term
	(:<=) :: Exp v Term -> Exp v Term -> Exp v Bool
	(:==) :: Exp v a -> Exp v a -> Exp v Bool

deriving instance Show v => Show (Exp v t)

instance Show v => Outputable (Exp v t) where
	ppr = text . show
