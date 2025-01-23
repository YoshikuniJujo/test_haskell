{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}

module Data.SwizzleSet.TH (swizzleSet) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Char

import Data.SwizzleSet.Class ()
import Data.SwizzleSet.Class.Pkg
import Template.Tools

swizzleSet :: String -> String -> DecsQ
swizzleSet pfx nm = sequence [xyzttd pfx nm, xyztfn pfx nm]

xyzttd :: String -> String -> DecQ
xyzttd pfx nm = newName "s" >>= \s -> newName `mapM` ((: "") <$> uvws) >>= \uvw ->
	sigD (mkFunName pfx nm) $
		forallT []
			(cxt (zipWith appT (zipWith appT
				(clsSwizzleXyz <$> nm) (tail $ scanr go (varT s) $ pairs uvw)) (varT <$> uvw)))
			(varT s `arrT` tupT uvw `arrT`
				foldr go (varT s) (pairs uvw))
	where
	go (xu, ul) = (`appT` ul) . (xu `appT`)
	pairs uvw = zip (typX <$> nm) (varT <$> uvw)
	uvws = crrPos ("xyz" ++ reverse ['a' .. 'w']) ("uvwxyz" ++ reverse ['a' .. 't']) <$> nm

clsSwizzleXyz :: Char -> TypeQ
clsSwizzleXyz = clsSwizzle
	. (+ 1) . fromJust . (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w']))

clsSwizzle :: Int -> TypeQ
clsSwizzle = conT . mkNameG_tc swizzleClassPkg "Data.SwizzleSet.Class.Base" . ("SwizzleSet" ++) . show

funX :: Char -> ExpQ
funX = varE . mkNameG_v swizzleClassPkg "Data.SwizzleSet.Class.Base" . (: "")

typX :: Char -> TypeQ
typX = conT . mkNameG_tc swizzleClassPkg "Data.SwizzleSet.Class.Base" . (: "") . toUpper

crrPos :: Eq a => [a] -> [b] -> a -> b
crrPos xs ys x = ys !! fromJust (x `L.elemIndex` xs)

xyztfn :: String -> String -> DecQ
xyztfn pfx nm =
	newName "s" >>= \s -> newName `mapM` ((: "") <$> uvws) >>= \uvw ->
	funD (mkFunName pfx nm) [
		clause [varP s, tupP $ varP <$> uvw] (normalB $
			foldr (\(xl, ul) -> (`appE` ul) . (xl `appE`)) (varE s) $
				zip (funX <$> nm) (varE <$> uvw)
--				zip (varE . mkName <$> ((: "") <$> nm)) (varE <$> uvw)
			) [] ]
	where
	uvws = crrPos ("xyz" ++ reverse ['a' .. 'w']) ("uvwxyz" ++ reverse ['a' .. 't']) <$> nm

mkFunName :: String -> String -> Name
mkFunName pfx nm = mkName case pfx of
	"" -> nm;
	_ -> case nm of h : t -> pfx ++ toUpper h : t; _ -> pfx
