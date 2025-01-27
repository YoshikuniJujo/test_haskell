{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleModify.TH (swizzleModify) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Char

import Data.SwizzleModify.Pkg qualified as Pkg
import Template.Tools

swizzleModify :: String -> String -> DecsQ
swizzleModify pfx nm = sequence [tdXy pfx nm, fnXy pfx nm]

mkFunName :: String -> String -> Name
mkFunName "" nm = mkName nm
mkFunName pfx (c : cs) = mkName $ pfx ++ toUpper c : cs
mkFunName _ "" = error "bad"

tdXy :: String -> String -> DecQ
tdXy pfx nm = newName "s" >>= \s -> (newName . (: "")) `mapM` xyab nm >>= \ab ->
	let	xsa : ysbs = swzsList s nm ab in
	sigD (mkFunName pfx nm)
		. forallT [] (cxt (
			zipWith appT (clsSwizzle <$> (idx <$> nm)) ysbs ++
			map (\(cs, (ysb', a')) -> cs `appT` ysb' `appT` varT a')
				((clsSwizzleSet <$> (idx <$> nm)) `zip` (ysbs `zip` ab))))
		$ tupT' (map (\(x, (ysb', a')) -> typX x `appT` ysb' `arrT` varT a')
				(nm `zip` (ysbs `zip` ab)))
			`arrT` varT s `arrT` xsa

idx :: Char -> Int
idx = (+ 1) . fromJust . (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w']))

xyab :: String -> String
xyab xy = (['a' .. 'z'] !!) . fromJust
	. (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w'])) <$> xy

swzsList :: Name -> String -> [Name] -> [TypeQ]
swzsList s xyz abc = scanr
	(\(x, a) t -> typSetX x `appT` t `appT` varT a) (varT s) (zip xyz abc)

fnXy :: String -> String -> DecQ
fnXy pfx nm = (newName . ('m' :) . (: "")) `mapM` nm >>= \ms ->
	funD (mkFunName pfx nm) [
		clause [tupP' $ varP <$> ms]
			(normalB . foldr1 comE $ zipWith (\x m -> funBX x `appE` varE m) nm ms) [] ]

funBX :: Char -> ExpQ
funBX = varE . mkNameG_v Pkg.swizzleModifyBasePkg "Data.SwizzleModify.Base" . (: "")
