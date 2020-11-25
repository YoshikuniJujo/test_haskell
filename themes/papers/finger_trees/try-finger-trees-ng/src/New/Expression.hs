{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Expression where

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
deriving instance Outputable (Exp v t)

type Given v = [(Exp v Bool, Bool)]
type Wanted v = (Exp v Bool, Bool)
