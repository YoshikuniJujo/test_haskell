{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Parts where

import Language.Haskell.TH (
	ExpQ, Exp(TupE), varE, litE, infixE, TypeQ, appT, tupleT, arrowT, PatQ, tupP,
	integerL, stringL )
import Data.Char (toLower, toUpper)

(.->) :: TypeQ -> TypeQ -> TypeQ
t1 .-> t2 = arrowT `appT` t1 `appT` t2

tupleE :: Int -> ExpQ
tupleE = \case 1 -> varE 'id; n -> pure . TupE $ n `replicate` Nothing

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT ts = foldl appT (tupleT $ length ts) ts

infixr 8 .$

(.$), (...), (.<$>), (.<*>), (.>>=), (.&&), (.||), (.==), (.<) :: ExpQ -> ExpQ -> ExpQ
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)
e1 ... e2 = infixE (Just e1) (varE '(.)) (Just e2)
e1 .<$> e2 = infixE (Just e1) (varE '(<$>)) (Just e2)
e1 .<*> e2 = infixE (Just e1) (varE '(<*>)) (Just e2)
e1 .>>= e2 = infixE (Just e1) (varE '(>>=)) (Just e2)
e1 .&& e2 = infixE (Just e1) (varE '(&&)) (Just e2)
e1 .|| e2 = infixE (Just e1) (varE '(||)) (Just e2)
e1 .== e2 = infixE (Just e1) (varE '(==)) (Just e2)
e1 .< e2 = infixE (Just e1) (varE '(<)) (Just e2)

(.+), (.*), zp :: ExpQ -> ExpQ -> ExpQ
e1 .+ e2 = infixE (Just e1) (varE '(+)) (Just e2)
e1 .* e2 = infixE (Just e1) (varE '(*)) (Just e2)
e1 `zp` e2 = infixE (Just e1) (varE 'zip) (Just e2)

pt :: ExpQ -> ExpQ -> ExpQ
e `pt` op = infixE (Just e) op Nothing

pp :: String -> ExpQ
pp s = litE (stringL s) `pt` varE '(++)

lcfirst, ucfirst :: String -> String
lcfirst = \case "" -> ""; c : cs -> toLower c : cs
ucfirst = \case "" -> ""; c : cs -> toUpper c : cs

litI :: Integer -> ExpQ
litI = litE . integerL
