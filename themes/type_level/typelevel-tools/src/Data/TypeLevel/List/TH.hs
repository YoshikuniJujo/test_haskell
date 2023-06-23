{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.List.TH where

import Prelude hiding (unzip)
import Language.Haskell.TH

unzip :: Int -> DecQ
unzip n = do
	xys <- newName $ concat abc ++ "s"
	xs <- newName `mapM` abc
	ks <- newName `mapM` (("k" ++) <$> abc)
	unzipGen (mkName $ "Unzip" ++ show n) (mkName $ "Push" ++ show n) xys xs ks
	where
	abc = take n varNames

unzipGen :: Name -> Name -> Name -> [Name] -> [Name] -> DecQ
unzipGen uz psh xys xs ks = do
	t <- (listT `appT`) . tupleFromList $ varT <$> ks
	closedTypeFamilyD uz [kindedTV xys t]
		noSig Nothing
		[tySynEqn Nothing lft0 rgt0, tySynEqn Nothing lft1 rgt1]
	where
	lft0 = conT uz `appT` promotedNilT
	rgt0 = promotedTupleFromList $ (const promotedNilT) <$> xs
	lft1 = appT (conT uz)
		$ promotedTupleFromList (varT <$> xs) `promotedCons` varT xys
	rgt1 = foldl appT (conT psh) (varT <$> xs) `appT` (conT uz `appT` varT xys)

push :: Int -> DecQ
push n = do
	xys <- newName $ concat abc ++ "s"
	xs <- newName `mapM` abc
	ks <- newName `mapM` (("k" ++) <$> abc)
	xss <- newName `mapM` ((++ "s") <$> abc)
	pushGen (mkName $ "Push" ++ show n) xys xs ks xss
	where
	abc = take n varNames

pushGen :: Name -> Name -> [Name] -> [Name] -> [Name] -> DecQ
pushGen psh xys xs ks xss = closedTypeFamilyD psh
	(zipWith kindedTV xs (varK <$> ks) ++ [plainTV xys]) NoSig Nothing [tySynEqn Nothing lft rgt ]
	where
	lft = foldl appT (conT psh) (varT <$> xs)
		`appT` promotedTupleFromList (varT <$> xss)
	rgt = promotedTupleFromList $ zipWith promotedCons (varT <$> xs) (varT <$> xss)

{-
type family Zip xs ys where
	Zip '[] _ys = '[]
	Zip _xs '[] = '[]
	Zip (x ': xs) (y ': ys) = '(x, y) ': Zip xs ys
	-}

tupleFromList :: Quote m => [m Type] -> m Type
tupleFromList ts = foldl appT (tupleT $ length ts) ts

promotedTupleFromList :: Quote m => [m Type] -> m Type
promotedTupleFromList ts = foldl appT (promotedTupleT $ length ts) ts

promotedCons :: Quote m => m Type -> m Type -> m Type
promotedCons x y = promotedConsT `appT` x `appT` y

varNames :: [String]
varNames = ((: []) <$> ['a' .. 'z']) ++ [ cs ++ [c] | cs <- varNames, c <- ['a' .. 'z'] ]
