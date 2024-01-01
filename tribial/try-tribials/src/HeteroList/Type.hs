{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList.Type where

import Data.Kind

infixr 5 :***

data PLC c (t :: k -> Type) (ss :: [k]) where
	Nil :: PLC c t '[]
	(:***) :: c s => t s -> PLC c t ss -> PLC c t (s ': ss)

instance Show (PLC c t '[]) where show Nil = "Nil"

instance (Show (t s), Show (PLC c t ss)) =>
	Show (PLC c t (s ': ss)) where
	show (x :*** xs) = show x ++ " :*** " ++ show xs
