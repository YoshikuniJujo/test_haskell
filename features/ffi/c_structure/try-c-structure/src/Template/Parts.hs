{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.Parts (
	tupleE, tupT, tupP', intE, strP,
	(.->), (.$), (...), (.<$>), (.<*>), (.>>=),
	(.&&), (.||), (.==), (.<), (.+), (.*), pt, zp, ss, (..+),
	toLabel, lcfirst ) where

import Language.Haskell.TH (
	ExpQ, Exp(TupE), varE, litE, infixE, TypeQ, appT, arrowT, tupleT,
	PatQ, litP, tupP, integerL, stringL )
import Data.Char (toLower, toUpper)

---------------------------------------------------------------------------

-- * TEMPLATE
--	+ TUPLE AND LITERAL
--	+ OPERATOR
--		- TYPE ARROW
--		- FUNCTION APPLICATION
--		- NORMAL OPERATOR
--		- PARTIAL AND ZIP
--	+ SHOW S
-- * CHARACTER

---------------------------------------------------------------------------
-- TEMPLATE
---------------------------------------------------------------------------

-- TUPLE AND LITERAL

tupleE :: Int -> ExpQ
tupleE = \case 1 -> varE 'id; n -> pure . TupE $ n `replicate` Nothing

tupT :: [TypeQ] -> TypeQ
tupT = \case [t] -> t; ts -> foldl appT (tupleT $ length ts) ts

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

intE :: Integer -> ExpQ
intE = litE . integerL

strP :: String -> PatQ
strP = litP . stringL

-- OPERATOR

-- TYPE ARROW

infixr 0 .->

(.->) :: TypeQ -> TypeQ -> TypeQ
t .-> u = arrowT `appT` t `appT` u

-- FUNCTION APPLICATION

infixr 0 .$
infixl 1 .>>=
infixl 4 .<$>, .<*>
infixr 8 ...

(.$), (...), (.<$>), (.<*>), (.>>=) :: ExpQ -> ExpQ -> ExpQ
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)
e1 ... e2 = infixE (Just e1) (varE '(.)) (Just e2)
e1 .<$> e2 = infixE (Just e1) (varE '(<$>)) (Just e2)
e1 .<*> e2 = infixE (Just e1) (varE '(<*>)) (Just e2)
e1 .>>= e2 = infixE (Just e1) (varE '(>>=)) (Just e2)

-- NORMAL OPERATOR

infixr 2 .||
infixr 3 .&&
infix 4 .==, .<

(.&&), (.||), (.==), (.<) :: ExpQ -> ExpQ -> ExpQ
e1 .&& e2 = infixE (Just e1) (varE '(&&)) (Just e2)
e1 .|| e2 = infixE (Just e1) (varE '(||)) (Just e2)
e1 .== e2 = infixE (Just e1) (varE '(==)) (Just e2)
e1 .< e2 = infixE (Just e1) (varE '(<)) (Just e2)
(.+), (.*) :: ExpQ -> ExpQ -> ExpQ
e1 .+ e2 = infixE (Just e1) (varE '(+)) (Just e2)
e1 .* e2 = infixE (Just e1) (varE '(*)) (Just e2)

-- PARTIAL AND ZIP

pt, zp :: ExpQ -> ExpQ -> ExpQ
e `pt` op = infixE (Just e) op Nothing
e1 `zp` e2 = infixE (Just e1) (varE 'zip) (Just e2)

-- SHOW S

ss :: String -> ExpQ
ss s = litE (stringL s) `pt` varE '(++)

(..+) :: String -> String -> ExpQ
s1 ..+ s2 = ss $ s1 ++ s2

---------------------------------------------------------------------------
-- CHARACTER
---------------------------------------------------------------------------

toLabel :: String -> String -> String
toLabel sn = (lcfirst sn ++) . ucfirst

lcfirst, ucfirst :: String -> String
lcfirst = \case "" -> ""; c : cs -> toLower c : cs
ucfirst = \case "" -> ""; c : cs -> toUpper c : cs
