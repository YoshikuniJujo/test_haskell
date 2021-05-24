{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Parts where

import Language.Haskell.TH (
	ExpQ, Exp(TupE), varE, litE, infixE, TypeQ, appT, tupleT, arrowT, PatQ, litP, tupP,
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

infixr 0 .->
infixr 0 .$
infixl 1 .>>=
infixr 2 .||
infixr 3 .&&
infix 4 .==, .<
infixl 4 .<$>, .<*>
infixr 8 ...

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

ss :: String -> ExpQ
ss s = litE (stringL s) `pt` varE '(++)

(..+) :: String -> String -> ExpQ
s1 ..+ s2 = ss $ s1 ++ s2

litI :: Integer -> ExpQ
litI = litE . integerL

strP :: String -> PatQ
strP = litP . stringL

toLabel :: String -> String -> String
toLabel sn = (lcfirst sn ++) . ucfirst

lcfirst, ucfirst :: String -> String
lcfirst = \case "" -> ""; c : cs -> toLower c : cs
ucfirst = \case "" -> ""; c : cs -> toUpper c : cs
