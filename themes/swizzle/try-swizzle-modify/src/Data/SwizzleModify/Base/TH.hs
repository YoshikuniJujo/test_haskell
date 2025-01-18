{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleModify.Base.TH (mkBase) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.List qualified as L
import Data.Char
import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS

import Data.SwizzleModify.Pkgs

x :: (Swz.Swizzle1 a, SwzS.SwizzleSet1 a, Swz.X a ~ SwzS.X a) =>
	(Swz.X a -> Swz.X a) -> a -> a
x f a = SwzS.x (f $ Swz.x a) a

y :: (Swz.Swizzle2 a, SwzS.SwizzleSet2 a, Swz.Y a ~ SwzS.Y a) =>
	(Swz.Y a -> Swz.Y a) -> a -> a
-- y f a = SwzS.y (f $ Swz.y a) a
y f = SwzS.y <$> (f . Swz.y) <*> id

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y = x <$> id <*> y

z :: (Swz.Swizzle3 a, SwzS.SwizzleSet3 a, Swz.Z a ~ SwzS.Z a) =>
	(Swz.Z a -> Swz.Z a) -> a -> a
z f a = SwzS.z (f $ Swz.z a) a

foo = 'SwzS.x

tdec0 :: DecsQ
tdec0 = [d|
	x :: (Swz.Swizzle1 a, SwzS.SwizzleSet1 a, Swz.X a ~ SwzS.X a) =>
		(Swz.X a -> Swz.X a) -> a -> a;
	x = undefined |]

swzSwizzleN :: Char -> TypeQ
swzSwizzleN c = case alphToN c of
	Nothing -> error "bad"
	Just n -> conT . mkNameG_tc swizzlePkg "Data.Swizzle.Class.Base" . ("Swizzle" ++) $ show n

swzsSwizzleSetN :: Char -> TypeQ
swzsSwizzleSetN c = case alphToN c of
	Nothing -> error "bad"
	Just n -> conT . mkNameG_tc swizzleSetPkg "Data.SwizzleSet.Class.Base" . ("SwizzleSet" ++) $ show n

swzXT :: Char -> TypeQ
swzXT c = conT . mkNameG_tc swizzlePkg "Data.Swizzle.Class.Base" . (: "") $ toUpper c

swzsXT :: Char -> TypeQ
swzsXT c = conT . mkNameG_tc swizzleSetPkg "Data.SwizzleSet.Class.Base" . (: "") $ toUpper c

swzXF :: Char -> ExpQ
swzXF = varE . mkNameG_v swizzlePkg "Data.Swizzle.Class.Base" . (: "")

swzsXF :: Char -> ExpQ
swzsXF = varE . mkNameG_v swizzleSetPkg "Data.SwizzleSet.Class.Base" . (: "")

mkBase :: Char -> DecsQ
mkBase c = sequence [tdec c, fun c]

tdec :: Char -> DecQ
tdec c = newName "a" >>= \a ->
	mkName (c : "") `sigD` (
		forallT [] (cxt [
			swzSwizzleN c `appT` varT a,
			swzsSwizzleSetN c `appT` varT a,
			swzXT c `appT` varT a `eqT` swzsXT c `appT` varT a
			]) $
		(swzXT c `appT` varT a `arrT` swzXT c `appT` varT a)
		`arrT` varT a `arrT` varT a )

fun0 :: DecsQ
fun0 = [d| x f a = SwzS.x (f $ Swz.x a) a |]

fun :: Char -> DecQ
fun c = newName "f" >>= \f -> newName "a" >>= \a ->
	funD (mkName $ c : "") [
		clause [varP f, varP a] (normalB
			$ swzsXF c `appE`
				(varE f `appE` (swzXF c `appE` varE a)) `appE`
				varE a)
			[]
		]

infixr 6 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

infixr 6 `eqT`

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

alphabet :: [Char]
alphabet = "xyz" ++ reverse ['a' .. 'w']

alphToN :: Char -> Maybe Int
alphToN c = (+ 1) <$> c `L.elemIndex` alphabet
