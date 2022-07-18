{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList.First where

infixr 5 `HeteroListCons`

data HeteroList as where
	HeteroListNil :: HeteroList '[]
	HeteroListCons :: a -> HeteroList as -> HeteroList (a ': as)

{-
instance Show (HeteroList as) where
	show = \case
		HeteroListNil -> "HeteroListNil"
		x `HeteroListCons` xs -> show x ++ " `HeteroListCons` " ++ show xs
		-}

instance Show (HeteroList '[]) where show HeteroListNil = "HeteroListNil"

instance (Show a, Show (HeteroList as)) => Show (HeteroList (a ': as)) where
	show (x `HeteroListCons` xs) = show x ++ " `HeteroListCons` " ++ show xs

class SumInt as where sumInt :: HeteroList as -> Integer

instance SumInt '[] where sumInt _ = 0

instance (Integral a, SumInt as) => SumInt (a ': as) where
	sumInt (x `HeteroListCons` xs) = fromIntegral x + sumInt xs
