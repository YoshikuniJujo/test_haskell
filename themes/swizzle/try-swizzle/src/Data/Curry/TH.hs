{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Curry.TH (crr, unc) where

import Language.Haskell.TH

crr :: Int -> DecsQ
crr n = (\a b -> [a, b])
	<$> crrSig n (take n $ (: "") <$> cycle ['a' .. 'z'])
	<*> crrFun n (take n $ (: "") <$> cycle ("xyz" ++ reverse ['a' .. 'w']))

crrSig :: Int -> [String] -> Q Dec
crrSig n ss = newName `mapM` ss >>= \vs -> newName "r" >>= \r -> sigD (mkName $ "crr" ++ show n)
	. foldr arrT (varT r) $ (tupT (varT <$> vs) `arrT` varT r) : (varT <$> vs)

crrFun :: Int -> [String] -> Q Dec
crrFun n ss = newName "f" >>= \f -> newName `mapM` ss >>= \vs ->
	funD (mkName $ "crr" ++ show n) [
		clause (varP f : (varP <$> vs))
			(normalB $ varE f `appE` tupE (varE <$> vs))
			[] ]

unc :: Int -> DecsQ
unc n = (\a b -> [a, b])
	<$> uncSig n (take n $ (: "") <$> cycle ['a' .. 'z'])
	<*> uncFun n (take n $ (: "") <$> cycle ("xyz" ++ reverse ['a' .. 'w']))

uncSig :: Int -> [String] -> Q Dec
uncSig n ss = newName `mapM` ss >>= \vs -> newName "r" >>= \r -> sigD (mkName $ "unc" ++ show n)
	$ (foldr arrT (varT r) $ varT <$> vs) `arrT` tupT (varT <$> vs) `arrT` varT r

uncFun :: Int -> [String] -> Q Dec
uncFun n ss = newName "f" >>= \f -> newName `mapM` ss >>= \vs ->
	funD (mkName $ "unc" ++ show n) [
		clause [varP f, tupP $ varP <$> vs]
			(normalB . foldl appE (varE f) $ varE <$> vs)
			[] ]

infixr 9 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts
