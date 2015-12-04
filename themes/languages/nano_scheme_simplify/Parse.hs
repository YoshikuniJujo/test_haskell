module Parse (Token, tokens, parse) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import Data.Char (isDigit, isAlpha, isSpace)

import Environment(
	Value(..), Symbol, ErrMsg,
	syntaxErr, tokenErr, readErr, strTrmErr, parseErr)

data Token
	= TkSym Symbol | TkInt Integer | TkDbl Double | TkStr String
	| TkTrue | TkFalse | OPrn | CPrn deriving Show

tokens :: String -> Either ErrMsg [Token]
tokens ('(' : s) = (OPrn :) <$> tokens s
tokens (')' : s) = (CPrn :) <$> tokens s
tokens ('#' : 'f' : s) = (TkFalse :) <$> tokens s
tokens ('#' : 't' : s) = (TkTrue :) <$> tokens s
tokens ('"' : s) = uncurry ($) . ((<$>) . (:) . TkStr *** tokens) =<< tkStr s
tokens str@(c : s)
	| isSymC c, let (sm, s') = (c :) `first` span isSymC s =
		(TkSym sm :) <$> tokens s'
	| isDigit c, let (ds, s') = (c :) `first` span isNumC s =
		(<$> tokens s') . (:) $ if '.' `elem` ds
			then TkDbl . read $ ds ++ "0"
			else TkInt $ read ds
	| isSpace c = tokens s
	| otherwise = Left $ syntaxErr ++ tokenErr ++ show str
	where
	isSymC = (`any` [isAlpha, (`elem` "+-*/<=>?")]) . flip ($)
	isNumC = (`any` [isDigit, (`elem` ".")]) . flip ($)
tokens _ = return []

tkStr :: String -> Either ErrMsg (String, String)
tkStr ('"' : s) = Right ("", s)
tkStr ('\\' : e : s) = (<$> tkStr s) . first . (:)
	$ case e of 't' -> '\t'; 'n' -> '\n'; 'r' -> '\r'; _ -> e
tkStr (c : s) = first (c :) <$> tkStr s
tkStr _ = Left $ readErr ++ strTrmErr

parse :: [Token] -> Either ErrMsg [Value]
parse [] = return []; parse ts = uncurry (<$>) . ((:) *** parse) =<< parse1 ts

parse1, parseCns :: [Token] -> Either ErrMsg (Value, [Token])
parse1 (TkSym s : ts) = return (Symbol s, ts)
parse1 (TkFalse : ts) = return (Bool False, ts)
parse1 (TkTrue : ts) = return (Bool True, ts)
parse1 (TkInt i : ts) = return (Int i, ts)
parse1 (TkDbl d : ts) = return (Dbl d, ts)
parse1 (TkStr s : ts) = return (Str s, ts)
parse1 (OPrn : ts) = parseCns ts
parse1 ts = Left $ syntaxErr ++ parseErr ++ show ts

parseCns (CPrn : ts) = return (Nil, ts)
parseCns ts = uncurry (<$>) . (first . Cons *** parseCns) =<< parse1 ts
