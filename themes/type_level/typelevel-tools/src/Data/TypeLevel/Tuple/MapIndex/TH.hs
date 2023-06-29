{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.MapIndex.TH (mkM, mkMTup, mTupIndices) where

import Language.Haskell.TH
import Data.List qualified as L

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

tuple' :: [Kind] -> Kind
tuple' ts = foldl appK (tupleK $ length ts) ts

promotedTuple :: [TypeQ] -> TypeQ
promotedTuple ts = foldl appT (promotedTupleT $ length ts) ts

mkMTup :: [Int] -> Int -> DecQ
mkMTup is n = do
	xyzs <- newName . concat $ take n varNames
	vs <- newName `mapM` take n varNames
	ks <- newName `mapM` take n kindNames
	rst <- newName . concat $ take n varNames
	bazRaw nm xyzs ks vs rst is
	where
	nm = mkName $ "M" ++ L.intercalate "'" (show <$> is) ++ "_" ++ show n

bazRaw :: Name -> Name -> [Name] -> [Name] -> Name -> [Int] -> DecQ
bazRaw tn xyzs ks vs rst is = closedTypeFamilyD tn
	[kindedTV xyzs $ listK `appK` (tuple' $ varK <$> ks)]
	noSig Nothing
	[	tySynEqn Nothing
			(conT tn `appT` promotedNilT)
			promotedNilT,
		tySynEqn Nothing
			(conT tn `appT` (
				promotedTuple (varT <$> vs) `promotedConsType`
					varT rst ))
			(promotedTuple (varT . (vs !!) <$> is) `promotedConsType`
				(conT tn `appT` varT rst))
		]

promotedConsType :: Quote m => m Type -> m Type -> m Type
promotedConsType a as = promotedConsT `appT` a `appT` as

varNames :: [String]
varNames = ((: "") <$> ['a' .. 'z']) ++ [ cs ++ [c] | cs <- varNames, c <- ['a' .. 'z'] ]

kindNames :: [String]
kindNames = ('k' :) . show <$> [0 :: Int ..]

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x : xs) = combinations xs ++ ((x :) <$> combinations xs)

mTupIndices :: Int -> [[Int]]
mTupIndices n = filter ((`notElem` [0, 1, n]) . length) $ combinations [0 .. n - 1]
