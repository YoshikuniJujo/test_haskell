{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Tools where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char

import Data.SwizzleModify.Base.Pkg qualified as Pkg

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

tupT' :: [TypeQ] -> TypeQ
tupT' = \case [n] -> n; ns -> foldl appT (tupleT $ length ns) ns

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

tupE' :: [ExpQ] -> ExpQ
tupE' = \case [e] -> e; es -> tupE es

infixr 7 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

infixr 7 `comE`

comE :: ExpQ -> ExpQ -> ExpQ
e1 `comE` e2 = infixE (Just e1) (varE '(.)) (Just e2)
