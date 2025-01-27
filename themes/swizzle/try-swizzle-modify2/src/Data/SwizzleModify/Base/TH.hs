{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleModify.Base.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char

import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS
import Data.SwizzleModify.Base.Pkg qualified as Pkg
import Template.Tools

mkX0 :: DecsQ
mkX0 = [d|
	x :: (Swz.Swizzle1 s, SwzS.SwizzleSet1 s b) => (Swz.X s -> b) -> s -> Swz.X s b
	x m s = SwzS.x s (m (Swz.x s))
	|]

mkX :: Int -> DecsQ
mkX n = sequence [tdX n c, fnX c]
	where c = ("xyz" ++ reverse ['a' .. 'w']) !! (n - 1)

tdX :: Int -> Char -> DecQ
tdX n c = newName "s" >>= \s -> newName "b" >>= \b ->
	sigD (mkName $ c : "") $
		forallT []
			(cxt [
				clsSwizzle n `appT` varT s,
				clsSwizzleSet n `appT` varT s `appT` varT b
				])
			((typX c `appT` varT s `arrT` varT b) `arrT`
				varT s `arrT`
				typSetX c `appT` varT s `appT` varT b)

fnX :: Char -> DecQ
fnX c = newName "m" >>= \m -> newName "s" >>= \s ->
	funD (mkName $ c : "") [clause
		[varP m, varP s]
		(normalB $
			funSetX c `appE`
				varE s `appE`
				(varE m `appE` (funX c `appE` varE s)))
		[]]
