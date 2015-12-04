module Parse (Token, tokens, parse) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import Data.Char (isDigit, isAlpha, isSpace)

import Environment(Value(..), Symbol, ErrMsg, stxErr, tknErr, prsErr)

data Token = TkSym Symbol | TkInt Integer | TkFls | TkTru | TkOPr | TkCPr
	deriving Show

tokens :: String -> Either ErrMsg [Token]
tokens ('(' : s) = (TkOPr :) <$> tokens s
tokens (')' : s) = (TkCPr :) <$> tokens s
tokens ('#' : 'f' : s) = (TkFls :) <$> tokens s
tokens ('#' : 't' : s) = (TkTru :) <$> tokens s
tokens str@(c : s)
	| isSymCh c, let (t, r) = span isSymCh s = (TkSym (c : t) :) <$> tokens r
	| isDigit c, let (t, r) = span isDigit s = (tkInt (c : t) :) <$> tokens r
	| isSpace c = tokens s | otherwise = Left $ stxErr ++ tknErr ++ show str
	where
	isSymCh = (`any` [isAlpha, (`elem` "+-*/<=>?")]) . flip ($)
	tkInt = TkInt . read
tokens _ = return []

parse :: [Token] -> Either ErrMsg [Value]
parse [] = return []; parse ts = uncurry (<$>) . ((:) *** parse) =<< parse1 ts

parse1, parseCns :: [Token] -> Either ErrMsg (Value, [Token])
parse1 (TkSym s : ts) = return (Symbol s, ts)
parse1 (TkInt i : ts) = return (Int i, ts)
parse1 (TkFls : ts) = return (B False, ts)
parse1 (TkTru : ts) = return (B True, ts)
parse1 (TkOPr : ts) = parseCns ts
parse1 ts = Left $ stxErr ++ prsErr ++ show ts

parseCns (TkCPr : ts) = return (Nil, ts)
parseCns ts = uncurry (<$>) . (first . Cons *** parseCns) =<< parse1 ts
