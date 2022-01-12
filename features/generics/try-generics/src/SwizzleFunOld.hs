{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SwizzleFunOld where

import Language.Haskell.TH
import Data.Char

import SwizzleGen

swizzle :: String -> DecsQ
swizzle nm = sequence [
	mkSwizzleSig i nm,
	mkSwizzleFun nm
	]
	where
	i = maximum $ unalphabet <$> nm

mkSwizzleSig :: Int -> String -> Q Dec
mkSwizzleSig i nm = sigD (mkName nm) . forallT [] (mkSwizzleSigContext i)
	$ varT (mkName "a_") `arrT` mkSwizzleSigTup nm (mkName "a_")

mkSwizzleSigContext :: Int -> CxtQ
mkSwizzleSigContext i = cxt [conT (nameSwizzle i) `appT` varT (mkName "a_")]

mkSwizzleSigTup :: String -> Name -> TypeQ
mkSwizzleSigTup cs a = tupT' $ (<$> cs) \c ->
	conT (mkName . (: "") $ toUpper c) `appT` varT a

tupT' :: [TypeQ] -> TypeQ
tupT' [t] = t
tupT' ts = foldl appT (tupleT $ length ts) ts

mkSwizzleFun :: String -> Q Dec
mkSwizzleFun nm = newName "a" >>= \a -> funD (mkName nm) [
	clause [varP a] (normalB $ mkSwizzleFunTup nm a) [] ]

mkSwizzleFunTup :: String -> Name -> ExpQ
mkSwizzleFunTup nm a = tupE $ (<$> nm) \c -> varE (mkName [c]) `appE` varE a
