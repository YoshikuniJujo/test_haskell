{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleSetClass2 where

import Try.SwizzleSetClass2.TH

import GHC.Generics
import Language.Haskell.TH
import Data.Bool
import Template.Tools

import Data.List qualified as L
import Data.Maybe
import Data.Char

concat <$> classSwizzle `mapM` [1 .. 4]

instance (
	GSwizzleSet2 (a :*: (b :*: c)) v, Foo (GY (a :*: (b :*: c)) v)
	) => GSwizzleSet2 ((a :*: b) :*: c) v where
	type GY ((a :*: b) :*: c) v = F' (GY (a :*: (b :*: c)) v)
	gy ((x :*: y) :*: z) v = f (gy @_ @v (x :*: (y :*: z)) v)

class Foo x where
	type F' x :: k -> *
	f :: x a -> F' x a

instance Foo (a :*: (b :*: c)) where
	type F' (a :*: (b :*: c)) = (a :*: b) :*: c
	f (x :*: (y :*: z)) = (x :*: y) :*: z

concat <$> instanceSwizzleTuple `mapM` [1 .. 2]

instance SwizzleSet2 (a, b, c) x where type Y (a, b, c) x = (a, x, c)

instance SwizzleSet1 (a, b, c, d) x where
	type X (a, b, c, d) x = (x, b, c, d)

instance SwizzleSet2 (a, b, c, d) x where
	type Y (a, b, c, d) x = (a, x, c, d)
--	y (x, y, z, w) a = (x, a, z, w)

-- xyzt "x"
xyzt "xy"
xyzt "xyz"

{-
xy :: (SwizzleSet2 s v, SwizzleSet1 (Y s v) w) => s -> (w, v) -> X (Y s v) w
xy s (w, v) = x (y s v) w

xyz :: (SwizzleSet1 (Y (Z s w) v) u, SwizzleSet2 (Z s w) v, SwizzleSet3 s w) =>
	s -> (u, v, w) -> X (Y (Z s w) v) u
xyz s (u, v, w) = x (y (z s w) v) u

foo :: (Show a, Show b, Show c) => [a] -> [b] -> c -> String
foo [] _ c = show c
foo _ [] c = show c
foo (x : xs) (y : ys) c = show x ++ " (" ++ foo xs ys c ++ ") " ++ show y

bar :: [String] -> [String] -> [String]
bar xs us =
	scanr (\(x, u) -> (++ (") " ++ u)) . ((x ++ " (") ++)) "s" $ zip xs us

xyzt0 :: DecsQ
xyzt0 = [d|
	xyz :: (SwizzleSet1 (Y (Z s w) v) u, SwizzleSet2 (Z s w) v, SwizzleSet3 s w) =>
		s -> (u, v, w) -> X (Y (Z s w) v) u
	xyz s (u, v, w) = x (y (z s w) v) u
	|]

xyzt :: String -> DecsQ
xyzt nm = sequence [xyzttd nm, xyztfn nm]

xyzttd :: String -> DecQ
xyzttd nm = newName "s" >>= \s -> newName `mapM` ((: "") <$> uvws) >>= \uvw ->
	sigD (mkName nm) $
		forallT []
			(cxt (zipWith appT (zipWith appT
				(conT . nameSwizzleXyz <$> nm) (tail $ scanr go (varT s) $ pairs uvw)) (varT <$> uvw)))
			(varT s `arrT` tupT uvw `arrT`
				foldr go (varT s) (pairs uvw))
	where
	go (xu, ul) = (`appT` ul) . (xu `appT`)
	pairs uvw = zip (conT . mkName <$> ((: "") . toUpper <$> nm)) (varT <$> uvw)
	uvws = crrPos ("xyz" ++ reverse ['a' .. 'w']) ("uvwxyz" ++ reverse ['a' .. 't']) <$> nm

crrPos :: Eq a => [a] -> [b] -> a -> b
crrPos xs ys x = ys !! fromJust (x `L.elemIndex` xs)

xyztfn :: String -> DecQ
xyztfn nm =
	newName "s" >>= \s -> newName `mapM` ((: "") <$> uvws) >>= \uvw ->
	funD (mkName nm) [
		clause [varP s, tupP $ varP <$> uvw] (normalB $
			foldr (\(xl, ul) -> (`appE` ul) . (xl `appE`)) (varE s) $
				zip (varE . mkName <$> ((: "") <$> nm)) (varE <$> uvw)
--			varE (mkName "xyz")
			) [] ]
	where
	uvws = crrPos ("xyz" ++ reverse ['a' .. 'w']) ("uvwxyz" ++ reverse ['a' .. 't']) <$> nm
	-}
