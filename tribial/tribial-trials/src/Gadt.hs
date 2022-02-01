{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gadt where

import Data.Kind

infixr 5 :::

data Hetero0 (as :: [Type]) where
	Nil :: Hetero0 '[]
	(:::) :: a -> Hetero0 as -> Hetero0 (a ': as)

instance Show (Hetero0 '[]) where
	show _ = "Nil"

instance (Show a, Show (Hetero0 as)) => Show (Hetero0 (a ': as)) where
	show (x ::: xs) = show x ++ " ::: " ++ show xs

data Pair a b c d = Pair a b c d Int deriving Show

infixr 5 ::::

data ComplexHetero (as :: ([(Type, Type)], [(Type, Type)])) where
	CNil :: ComplexHetero '( '[], '[])
	(::::) :: Pair a b c d -> ComplexHetero '(as, cs) ->
		ComplexHetero '( '(a, b) ': as, '(c, d) ': cs)

instance Show (ComplexHetero '( '[], '[])) where
	show _ = "CNil"

instance (Show a, Show b, Show c, Show d, Show (ComplexHetero '(as, cs))) =>
	Show (ComplexHetero '( '(a, b) ': as, '(c, d) ': cs)) where
	show (p :::: ps) = show p ++ " :::: " ++ show ps

sumComplexHetero :: ComplexHetero as -> [Int]
sumComplexHetero CNil = []
sumComplexHetero (Pair _ _ _ _ a :::: ps) = a : sumComplexHetero ps

-- newtype IntAndTypes 
