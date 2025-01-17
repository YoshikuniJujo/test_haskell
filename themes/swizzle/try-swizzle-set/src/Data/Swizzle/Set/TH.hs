{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Swizzle.Set.TH (swizzleSet) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Char

import Data.Swizzle.Set.Class.Pkg

swizzleSet :: String -> DecsQ
swizzleSet nm = sequence [mkSwizzleSig i nm, mkSwizzleFun nm]
	where i = maximum $ unalphabet <$> nm

mkSwizzleSig :: Int -> String -> Q Dec
mkSwizzleSig i nm = sigD (mkName nm) . forallT [] (mkSwizzleSigContext i) $
	mkSwizzleSigTup nm (mkName "a")
	`arrT`
	varT (mkName "a")
	`arrT`
	varT (mkName "a")

mkSwizzleSigContext :: Int -> CxtQ
mkSwizzleSigContext i = cxt [clsSwizzle i `appT` varT (mkName "a")]

mkSwizzleSigTup :: String -> Name -> TypeQ
mkSwizzleSigTup cs a = tupT $ (<$> cs) \c -> typX c `appT` varT a

clsSwizzle :: Int -> TypeQ
clsSwizzle = conT . mkNameG_tc swizzleClassPkg "Data.Swizzle.Set.Class.Base" . ("SwizzleSet" ++) . show

funX :: Char -> ExpQ
funX = varE . mkNameG_v swizzleClassPkg "Data.Swizzle.Set.Class.Base" . (: "")

typX :: Char -> TypeQ
typX = conT . mkNameG_tc swizzleClassPkg "Data.Swizzle.Set.Class.Base" . (: "") . toUpper

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

unalphabet :: Char -> Int
unalphabet c = fromJust (L.elemIndex c $ ("xyz" ++ reverse ['a' .. 'w'])) + 1

infixr 7 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

mkSwizzleFun :: String -> Q Dec
mkSwizzleFun nm =
	((newName . (: "")) `mapM` nm) >>= \vs ->
	funD (mkName nm) [
	clause [tupP $ varP <$> vs] (normalB $ mkSwizzleFunTup nm vs) []
	]

mkSwizzleFunTup :: String -> [Name] -> ExpQ
mkSwizzleFunTup nm vs = foldr1 comE $ (<$> zip nm vs) \(c, v) -> funX c `appE` varE v

infixr 7 `comE`

comE :: ExpQ -> ExpQ -> ExpQ
e1 `comE` e2 = infixE (Just e1) (varE '(.)) (Just e2)
