{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle.TH (swizzle) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List
import Data.Char

import Data.Swizzle.Class.Pkg

swizzle :: String -> DecsQ
swizzle nm = sequence [mkSwizzleSig i nm, mkSwizzleFun nm]
	where i = maximum $ unalphabet <$> nm

mkSwizzleSig :: Int -> String -> Q Dec
mkSwizzleSig i nm = sigD (mkName nm) . forallT [] (mkSwizzleSigContext i)
	$ varT (mkName "a") `arrT` mkSwizzleSigTup nm (mkName "a")

mkSwizzleSigContext :: Int -> CxtQ
mkSwizzleSigContext i = cxt [clsSwizzle i `appT` varT (mkName "a")]

mkSwizzleSigTup :: String -> Name -> TypeQ
mkSwizzleSigTup cs a = tupT $ (<$> cs) \c -> typX c `appT` varT a

clsSwizzle :: Int -> TypeQ
clsSwizzle = conT . mkNameG_tc swizzleClassPkg "Data.Swizzle.Class.Base" . ("Swizzle" ++) . show

funX :: Char -> ExpQ
funX = varE . mkNameG_v swizzleClassPkg "Data.Swizzle.Class.Base" . (: "")

typX :: Char -> TypeQ
typX = conT . mkNameG_tc swizzleClassPkg "Data.Swizzle.Class.Base" . (: "") . toUpper

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

unalphabet :: Char -> Int
unalphabet c = fromJust (elemIndex c $ ("xyz" ++ reverse ['a' .. 'w'])) + 1

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

mkSwizzleFun :: String -> Q Dec
mkSwizzleFun nm = newName "a" >>= \a -> funD (mkName nm) [
	clause [varP a] (normalB $ mkSwizzleFunTup nm a) [] ]

mkSwizzleFunTup :: String -> Name -> ExpQ
mkSwizzleFunTup nm a = tupE $ (<$> nm) \c -> funX c `appE` varE a
