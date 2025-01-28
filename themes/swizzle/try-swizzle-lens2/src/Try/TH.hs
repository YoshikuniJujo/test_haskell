{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Char
import Data.SwizzleLens.Pkg qualified as Pkg
import Template.Tools

mkXy :: String -> DecsQ
mkXy nm = sequence [tdXy nm, fnXy nm]

tdXy :: String -> DecQ
tdXy nm = newName "s" >>= \s ->
	(newName . (: "")) `mapM` (xyzAbc <$> nm) >>= \ab ->
	newName "f" >>= \f ->
	let	xsa : ysbs = swzsList s nm ab in
	sigD (mkName nm) $ forallT []
		(cxt (	[	clsSwizzle (maxIdx nm) `appT` varT s ] ++
			map (\(cs, (ysb', a')) -> cs `appT` ysb' `appT` varT a')
				((clsSwizzleSet <$> (idx <$> nm)) `zip` (ysbs `zip` ab)) ++
			[	conT ''Functor `appT` varT f ]))
		((	tupT' ((`appT` varT s) . typX <$> nm) `arrT`
			varT f `appT` tupT' (varT <$> ab) ) `arrT`
		varT s `arrT`
		varT f `appT` xsa)

xyzAbc :: Char -> Char
xyzAbc c = (['a' .. 'z'] !!) . fromJust $ c `L.elemIndex` ("xyz" ++ reverse ['a' .. 'w'])

maxIdx :: String -> Int
maxIdx cs = maximum $ mapMaybe
	(((+ 1) <$>) . (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w']))) cs

idx :: Char -> Int
idx = (+ 1) . fromJust . (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w']))

swzsList :: Name -> String -> [Name] -> [TypeQ]
swzsList s xyz abc = scanr
	(\(x, a) t -> typSetX x `appT` t `appT` varT a) (varT s) (zip xyz abc)

fnXy :: String -> DecQ
fnXy nm = newName "f" >>= \f -> newName "s" >>= \s ->
	newName "st" >>= \st -> (newName . (: "")) `mapM` (xyzAbc <$> nm) >>= \ab ->
	funD (mkName nm) [
		clause [varP f, varP s]
			(normalB $
				varE st `fmapE` varE f `appE`
					(tupE' $ (`appE` varE s) . funX <$> nm)
				)
			[funD st [clause [tupP' $ varP <$> ab]
				(normalB $ foldr (\(x, a) -> (`appE` a) . (x `appE`)) (varE s) $
					zip (funSetX <$> nm) (varE <$> ab))
				[]]]
		]


clsSwizzle :: Int -> TypeQ
clsSwizzle = conT
	. mkNameG_tc Pkg.swizzleClassPkg "Data.Swizzle.Class.Base"
	. ("Swizzle" ++) . show

typX :: Char -> TypeQ
typX = conT
	. mkNameG_tc Pkg.swizzleClassPkg "Data.Swizzle.Class.Base"
	. (: "") . toUpper

funX :: Char -> ExpQ
funX = varE . mkNameG_v Pkg.swizzleClassPkg "Data.Swizzle.Class.Base" . (: "")

clsSwizzleSet :: Int -> TypeQ
clsSwizzleSet = conT
	. mkNameG_tc Pkg.swizzleSetClassPkg "Data.SwizzleSet.Class.Base"
	. ("SwizzleSet" ++) . show

typSetX :: Char -> TypeQ
typSetX = conT
	. mkNameG_tc Pkg.swizzleSetClassPkg "Data.SwizzleSet.Class.Base"
	. (: "") . toUpper

funSetX :: Char -> ExpQ
funSetX = varE
	. mkNameG_v Pkg.swizzleSetClassPkg "Data.SwizzleSet.Class.Base"
	. (: "")
