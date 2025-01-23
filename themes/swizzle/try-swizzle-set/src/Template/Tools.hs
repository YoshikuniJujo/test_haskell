{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Tools (
	Q, Dec, Name, newName,

	conT, varT, appT, arrT, arrK, eqT, tupT,
	tupT', tupP', tupE',

	sigD, classD, openTypeFamilyD,

	noSig, plainTV, cxt,

	nameSwizzle, nameSwizzleXyz,
	nameGswizzle, nameGxU, nameGxL, nameXU,

	prodT, prodP, prodE

	) where

import GHC.Generics
import Language.Haskell.TH
import Data.Maybe
import Data.List qualified as L
import Data.Char

nameSwizzleXyz :: Char -> Name
nameSwizzleXyz = nameSwizzle
	. (+ 1) . fromJust . (`L.elemIndex` ("xyz" ++ reverse ['a' .. 'w']))

nameSwizzle :: Int -> Name
nameSwizzle = mkName . ("SwizzleSet" ++) . show

nameXU :: Int -> Name
nameXU = mkName . (: "") . toUpper . alphabet

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

tupT :: [Name] -> TypeQ
tupT ns = foldl appT (tupleT $ length ns) $ varT <$> ns

tupT' :: [TypeQ] -> TypeQ
tupT' = \case [n] -> n; ns -> foldl appT (tupleT $ length ns) ns

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

tupE' :: [ExpQ] -> ExpQ
tupE' = \case [e] -> e; es -> tupE es

nameGswizzle :: Int -> Name
nameGswizzle = mkName . ("GSwizzleSet" ++) . show

nameGxU :: Int -> Name
nameGxU i = mkName $ "G" ++ [toUpper $ alphabet i]

nameGxL :: Int -> Name
nameGxL i = mkName $ "g" ++ [alphabet i]

infixr 9 `prodT`, `prodE`, `prodP`

prodT :: TypeQ -> TypeQ -> TypeQ
t1 `prodT` t2 = conT ''(:*:) `appT` t1 `appT` t2

prodE :: ExpQ -> ExpQ -> ExpQ
e1 `prodE` e2 = conE '(:*:) `appE` e1 `appE` e2

prodP :: PatQ -> PatQ -> PatQ
p1 `prodP` p2 = infixP p1 '(:*:) p2

alphabet :: Int -> Char
alphabet i | i > 26 = error $ "no such alphabet: " ++ show i
alphabet i = (("xyz" ++ reverse ['a' .. 'w']) !!) $ subtract 1 i

infixr 7 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

infixr 6 `arrK`

arrK :: Kind -> Kind -> Kind
k1 `arrK` k2 = arrowK `appK` k1 `appK` k2
