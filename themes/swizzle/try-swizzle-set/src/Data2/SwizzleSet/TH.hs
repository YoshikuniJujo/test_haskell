{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data2.SwizzleSet.TH (swizzleSet) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Char

import Data2.SwizzleSet.Class.Pkg

swizzleSet :: String -> String -> DecsQ
swizzleSet pfx nm = sequence [mkSwizzleSig i pfx nm, mkSwizzleFun pfx nm]
	where i = maximum $ unalphabet <$> nm

mkSwizzleSig :: Int -> String -> String -> Q Dec
mkSwizzleSig i pfx nm = sigD (mkFunName pfx nm) . forallT [] (mkSwizzleSigContext i) $
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
clsSwizzle = conT . mkNameG_tc swizzleClassPkg "Data2.SwizzleSet.Class.Base" . ("SwizzleSet" ++) . show

funX :: Char -> ExpQ
funX = varE . mkNameG_v swizzleClassPkg "Data2.SwizzleSet.Class.Base" . (: "")

typX :: Char -> TypeQ
typX = conT . mkNameG_tc swizzleClassPkg "Data2.SwizzleSet.Class.Base" . (: "") . toUpper

tupT :: [TypeQ] -> TypeQ
tupT = \case [t] -> t; ts -> foldl appT (tupleT $ length ts) ts

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

unalphabet :: Char -> Int
unalphabet c = fromJust (L.elemIndex c $ ("xyz" ++ reverse ['a' .. 'w'])) + 1

infixr 7 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

mkSwizzleFun :: String -> String -> Q Dec
mkSwizzleFun pfx nm =
	((newName . (: "")) `mapM` nm) >>= \vs ->
	funD (mkFunName pfx nm) [
	clause [tupP' $ varP <$> vs] (normalB $ mkSwizzleFunTup nm vs) []
	]

mkSwizzleFunTup :: String -> [Name] -> ExpQ
mkSwizzleFunTup nm vs = foldr1 comE $ (<$> zip nm vs) \(c, v) -> funX c `appE` varE v

infixr 7 `comE`

comE :: ExpQ -> ExpQ -> ExpQ
e1 `comE` e2 = infixE (Just e1) (varE '(.)) (Just e2)

mkFunName :: String -> String -> Name
mkFunName pfx nm = mkName case pfx of
	"" -> nm;
	_ -> case nm of h : t -> pfx ++ toUpper h : t; _ -> pfx
