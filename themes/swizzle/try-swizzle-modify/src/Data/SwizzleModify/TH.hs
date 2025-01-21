{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleModify.TH (swizzleModify) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.List qualified as L
import Data.Maybe
import Data.Char
import Data.SwizzleModify.Pkg
import Data.SwizzleModify.Pkgs

swizzleModify :: String -> DecsQ
swizzleModify nm = sequence [tdep nm, fun nm]

tdep :: String -> DecQ
tdep nm = newName "a" >>= \a ->
	mkName nm `sigD` (
	forallT [] (cxt $ [
		swzSwizzleN (maxN nm) `appT` varT a,
		swzsSwizzleSetN (maxN nm) `appT` varT a ] ++
		((`teq` a) <$> nm))
	$ tupT ((`fn` a) <$> nm) `arrT` varT a `arrT` varT a )
	where
	teq c a = swzXT c `appT` varT a `eqT` swzsXT c `appT` varT a
	fn c a = swzXT c `appT` varT a `arrT` swzXT c `appT` varT a

fun :: String -> DecQ
fun nm = newName `mapM` (('f' :) . (: "") <$> nm) >>= \fs ->
	funD (mkName nm) [
		clause [tupP' $ varP <$> fs] (normalB
			$ foldr1 comE (uncurry f1 <$> zip nm fs)
			) []
		]
	where f1 c f = swzmXF c `appE` varE f

swzSwizzleN :: Int -> TypeQ
swzSwizzleN n =
	conT . mkNameG_tc swizzlePkg "Data.Swizzle.Class.Base" . ("Swizzle" ++) $ show n

swzsSwizzleSetN :: Int -> TypeQ
swzsSwizzleSetN n =
	conT . mkNameG_tc swizzleSetPkg "Data.SwizzleSet.Class.Base" . ("SwizzleSet" ++) $ show n

swzXT :: Char -> TypeQ
swzXT = conT . mkNameG_tc swizzlePkg "Data.Swizzle.Class.Base" . (: "") . toUpper

swzsXT :: Char -> TypeQ
swzsXT = conT . mkNameG_tc swizzleSetPkg "Data.SwizzleSet.Class.Base" . (: "") . toUpper

swzmXF :: Char -> ExpQ
swzmXF = varE . mkNameG_v swizzleModifyPkg "Data.SwizzleModify.Base" . (: "")

tupT :: [TypeQ] -> TypeQ
tupT = \case [t] -> t; ts -> foldl appT (tupleT $ length ts) ts

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

infixr 6 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT ` t2 = arrowT `appT` t1 `appT` t2

alphabet :: [Char]
alphabet = "xyz" ++ reverse ['a' .. 'w']

alphToN :: Char -> Maybe Int
alphToN c = (+ 1) <$> c `L.elemIndex` alphabet

maxN :: [Char] -> Int
maxN = maximum . catMaybes . (alphToN <$>)

infixr 6 `eqT`

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

infixr 7 `comE`

comE :: ExpQ -> ExpQ -> ExpQ
e1 `comE` e2 = infixE (Just e1) (varE '(.)) (Just e2)
