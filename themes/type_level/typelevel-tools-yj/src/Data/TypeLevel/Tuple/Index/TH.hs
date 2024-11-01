{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.Index.TH (mkI) where

import Language.Haskell.TH

mkI :: Int -> Int -> DecQ
mkI i n = do
	hd <- newName . concat $ take n varNames
	ks <- newName `mapM` take n kindNames
	vs <- newName `mapM` take n varNames
	rawBar tn hd ks vs i
	where tn = mkName $ "I" ++ show i ++ "_" ++ show n

rawBar :: Name -> Name -> [Name] -> [Name] -> Int -> DecQ
rawBar tn hd ks vs i = closedTypeFamilyD tn
	[kindedTV hd
		(tupleKind $ varK <$> ks)]
	NoSig Nothing
	[tySynEqn Nothing
		(conT tn `appT` promotedTupleType (varT <$> vs))
		(varT $ vs !! i)]

promotedTupleType :: Quote m => [m Type] -> m Type
promotedTupleType ts = foldl appT (promotedTupleT $ length ts) ts

tupleKind :: [Kind] -> Kind
tupleKind ks = foldl appK (tupleK $ length ks) ks

varNames :: [String]
varNames = ((: "") <$> ['a' .. 'z']) ++
	[ cs ++ [c] | cs <- varNames, c <- ['a' .. 'z'] ]

kindNames :: [String]
kindNames = ('k' :) . show <$> [0 :: Int ..]
