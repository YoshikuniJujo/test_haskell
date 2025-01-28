{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Tools where

import Language.Haskell.TH

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

infixr 5 `fmapE`

fmapE :: ExpQ -> ExpQ -> ExpQ
e1 `fmapE` e2 = infixE (Just e1) (varE '(<$>)) (Just e2)
