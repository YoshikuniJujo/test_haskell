{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroParList.Constrained where

import Data.Kind

data PL c (t :: k -> Type) (ss :: [k]) where
	Nil :: PL c t '[]
	(:***) :: c s => t s -> PL c t ss -> PL c t (s ': ss)

instance Show (PL c t '[]) where show Nil = "Nil"

instance (Show (t s), Show (PL c t ss)) =>
	Show (PL c t (s ': ss)) where
	show (x :*** xs) = show x ++ " :*** " ++ show xs
