{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
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

concat <$> classSwizzle `mapM` [1 .. 3]

concat <$> instanceSwizzleTuple `mapM` [1 .. 3]

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

xyzttd :: DecQ
xyzttd = newName "s" >>= \s -> newName `mapM` ["u", "v", "w"] >>= \[u, v, w] ->
	sigD (mkName "xyz")
		(varT s `arrT` tupT [u, v, w] `arrT`
			foldr (\(xu, ul) -> (`appT` ul) . (xu `appT`)) (varT s)
				(zip (conT . mkName <$> ["X", "Y", "Z"]) (varT <$> [u, v, w])))
