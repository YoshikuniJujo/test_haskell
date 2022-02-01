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

complexHeteroNums :: ComplexHetero as -> [Int]
complexHeteroNums CNil = []
complexHeteroNums (Pair _ _ _ _ a :::: ps) = a : complexHeteroNums ps

newtype IntAndTypes c d = IntAndTypes Int deriving Show

data IntAndTypesList (as :: [(Type, Type)]) where
	INil :: IntAndTypesList '[]
	(:-:) :: IntAndTypes c d -> IntAndTypesList cs ->
		IntAndTypesList ('(c, d) ': cs)

instance Show (IntAndTypesList '[]) where
	show _ = "INil"

instance Show (IntAndTypesList as) => Show (IntAndTypesList (a ': as)) where
	show (x :-: xs) = show x ++ " :-: " ++ show xs

complexHeteroNumsToIntAndTypesList :: ComplexHetero '(as, cs) -> IntAndTypesList cs
complexHeteroNumsToIntAndTypesList CNil = INil
complexHeteroNumsToIntAndTypesList (Pair _ _ _ _ x :::: ps) =
	IntAndTypes x :-: complexHeteroNumsToIntAndTypesList ps

class ListAddTypes (as :: [(Type, Type)]) where
	listAddTypes :: [Int] -> IntAndTypesList as

instance ListAddTypes '[] where
	listAddTypes [] = INil
	listAddTypes _ = error "extra ints"

instance ListAddTypes as => ListAddTypes ('(c, d) ': as) where
-- instance ListAddTypes as => ListAddTypes ((a :: (Type, Type)) ': as) where
	listAddTypes [] = error "ints is not enough"
	listAddTypes (i : is) = IntAndTypes i :-: listAddTypes is

complexHeteroNumsToIntAndTypeList' :: ListAddTypes cs =>
	ComplexHetero '(as, cs) -> IntAndTypesList cs
complexHeteroNumsToIntAndTypeList' = listAddTypes . complexHeteroNums
