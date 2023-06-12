{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.MapIndex.TH (mkM) where

import Language.Haskell.TH

vars :: [String]
vars = mkVar `concatMap` [1 ..]
	where
	mkVar :: Int -> [String]
	mkVar 0 = [""]
	mkVar n = [ cs ++ [c] | cs <- mkVar (n - 1), c <- ['a' .. 'z'] ]

names :: [Q Name]
names = newName <$> vars

mkM :: Int -> Int -> Q Dec
mkM i n = mkFoo' (mkName nm) i (take n names) -- [varT $ mkName "a", varT $ mkName "b"]
	where
	nm = 'M' : show i ++ "_" ++ show n

mkFoo' :: Name -> Int -> [Q Name] -> Q Dec
mkFoo' nm i nms = do
	ts <- (VarT <$>) <$> sequence nms
	ts' <- (varT <$>) <$> sequence nms
	closedTypeFamilyD nm
		[	kindedTV (mkName "tpl") (ListT `AppT` tuple ts) ] -- [VarT $ mkName "a", VarT $ mkName "b"]) ]
		(KindSig $ ListT `AppT` (ts !! i))
		Nothing
		[	tySynEqn Nothing (conT nm `appT` promotedNilT) promotedNilT,
			tySynEqn Nothing (conT nm `appT` bar ts')
				$ promotedT '(:) `appT` (ts' !! i) `appT` (conT nm `appT` varT (mkName "rst"))
			]

bar :: [TypeQ] -> TypeQ
bar ts = promotedT '(:) `appT` promotedTuple ts `appT` (varT $ mkName "rst")

tuple :: [Type] -> Type
tuple ts = foldl AppT (TupleT $ length ts) ts

promotedTuple :: [TypeQ] -> TypeQ
promotedTuple ts = foldl appT (promotedTupleT $ length ts) ts

{-
type family M0_5 (tpl :: [(i, j, k, l, m)]) :: [i] where
	M0_5 '[] = '[]
	M0_5 ('(x, y, z, w, v) ': xyzwvs) = x ': M0_5 xyzwvs
	-}
