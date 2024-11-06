{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroParList.Tuple.TH where

import Control.Monad
import Language.Haskell.TH

import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

mkMap :: Int -> Int -> DecsQ
mkMap i n = do
	ts <- newName `mapM` take n typeVarNames
	ks <- newName `mapM` take n kindVarNames
	barAll cn fn ts ks i
	where
	cn = mkName $ "Map" ++ show i ++ "_" ++ show n
	fn = mkName $ "map" ++ show i ++ "_" ++ show n

barAll :: Name -> Name -> [Name] -> [Name] -> Int -> DecsQ
barAll cn fn ts ks i =
	sequence [barRaw cn fn ts ks i, barRaw0 cn fn, barRaw1 cn fn ts]

bar :: DecQ
bar = barRaw (mkName "Map0_2") (mkName "map0_2") [mkName "a", mkName "b"] [mkName "k0", mkName "k1"] 0

barRaw :: Name -> Name -> [Name] -> [Name] -> Int -> DecQ
barRaw cn fn ts ks i = classD (pure []) cn
	[kindedTV (mkName "ss") sskind] [] [sigD fn map0_2type]
	where
	sskind = listKind . tupleKind $ varK <$> ks
	map0_2type = ftype `arrT` (stype `arrT` dtype)
	ftype = do
		abcs <- zipWithM kindedInvis ts ks
		forallT	abcs (pure []) (tab `arrT` tb)
	kindedInvis v t = kindedInvisTV v SpecifiedSpec . pure $ varK t
	tab = varT (mkName "t") `appT` promotedTupleType (varT <$> ts)
	tb = varT (mkName "t'") `appT` varT (ts !! i)
	stype = pl `appT` varT (mkName "t") `appT` varT (mkName "ss")
	dtype = pl `appT` varT (mkName "t'") `appT` (m0_2 i (length ts) `appT` varT (mkName "ss"))

bar0 :: DecQ
bar0 = barRaw0 (mkName "Map0_2") (mkName "map0_2")

barRaw0 :: Name -> Name -> DecQ
barRaw0 cn fn = instanceD (cxt [])
	(conT cn `appT` promotedNilT)
	[funD fn [clause [wildP, conP 'HeteroParList.Nil []] (normalB $ conE 'HeteroParList.Nil) []]]

bar1 :: DecQ
bar1 = barRaw1 (mkName "Map0_2") (mkName "map0_2") [mkName "a", mkName "b"]

barRaw1 :: Name -> Name -> [Name] -> DecQ
barRaw1 cn fn ts = instanceD (cxt [conT cn `appT` varT (mkName "ts")])
	(conT cn `appT`
		(promotedTupleType (varT <$> ts) `promotedConsType`
			varT (mkName "ts")))
	[funD fn [clause
		[varP $ mkName "f", varP (mkName "x") `heteroConsP` varP (mkName "xs")]
		(normalB $ varE (mkName "f") `appE` varE (mkName "x") `heteroCons`
			(varE fn `appE` varE (mkName "f") `appE` varE (mkName "xs")))
		[]]]

pl :: TypeQ
pl = conT ''HeteroParList.PL

m0_2 :: Int -> Int -> TypeQ
-- m0_2 i n = conT $ mkNameG_tc "typelevel-tools" "Data.TypeLevel.MapIndex" ("M" ++ show i ++ "_" ++ show n)
m0_2 i n = conT . mkName $ "TMapIndex.M" ++ show i ++ "_" ++ show n

tupleKind :: [Kind] -> Kind
tupleKind ks = foldl appK (tupleK $ length ks) ks

listKind :: Kind -> Kind
listKind = (listK `appK`)

arrT :: Quote m => m Type -> m Type -> m Type
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

promotedTupleType :: Quote m => [m Type] -> m Type
promotedTupleType ts = foldl appT (promotedTupleT $ length ts) ts

promotedConsType :: Quote m => m Type -> m Type -> m Type
promotedConsType t ts = promotedConsT `appT` t `appT` ts

heteroCons :: Quote m => m Exp -> m Exp -> m Exp
heteroCons x xs = infixE (Just x) (conE '(:**)) (Just xs)

heteroConsP :: Quote m => m Pat -> m Pat -> m Pat
heteroConsP p ps = infixP p '(:**) ps

typeVarNames :: [String]
typeVarNames = ((: "") <$> ['a' .. 'z']) ++
	[ cs ++ [c] | cs <- typeVarNames, c <- ['a' .. 'z'] ]

kindVarNames :: [String]
kindVarNames = ('k' :) . show <$> [0 :: Int ..]
