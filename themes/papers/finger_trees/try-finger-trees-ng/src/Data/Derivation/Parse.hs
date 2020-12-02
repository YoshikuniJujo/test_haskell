{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse where

import Control.Applicative hiding (Const)
import Data.Function
import Data.Maybe
import Data.List
import Data.Char

import Data.Derivation.Expression
import Internal.Tools.Parse

type Op = Exp String Term -> Exp String Term -> Exp String Term

bool :: Parse String (Exp String Bool)
bool =	(Bool False <$ token "F") <|>
	(Bool True <$ token "T") <|>
	leq -- <|>
--	eq

leq :: Parse String (Exp String Bool)
leq = (:<=) <$> (term <* token "<=") <*> term

eq :: Parse String (Exp String Bool)
eq = undefined

term :: Parse String (Exp String Term)
term = (\x oys -> foldl (&) x oys) <$> num <*> many opNum

opNum :: Parse String (Exp String Term -> Exp String Term)
opNum = (\op n -> (`op` n)) <$> adsb <*> num

adsb :: Parse String Op
adsb = ((:+) <$ token "+") <|> ((:-) <$ token "-")

num :: Parse String (Exp String Term)
num =	(Const . read <$> check (all isDigit)) <|>
	(Var <$> check (all isLower))

tokens :: String -> [String]
tokens = unfoldr lex'

lex' :: String -> Maybe (String, String)
lex' "" = Nothing
lex' s = listToMaybe $ lex s
