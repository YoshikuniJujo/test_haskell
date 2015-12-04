module Parse (Token, tokens, parse) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import Data.Char (isDigit, isAlpha, isSpace)

import Environment(
	Value(..), Symbol, Error(..),
	syntaxErr, tokenErr, readErr, strTrmErr, parseErr)

data Token
	= TkSym Symbol | TkInt Integer | TkDbl Double | TkStr String
	| TkTrue | TkFalse | OParen | CParen deriving Show

tokens :: String -> Either Error [Token]
tokens ('(' : s) = (OParen :) <$> tokens s
tokens (')' : s) = (CParen :) <$> tokens s
tokens ('#' : 'f' : s) = (TkFalse :) <$> tokens s
tokens ('#' : 't' : s) = (TkTrue :) <$> tokens s
tokens ('"' : s) = uncurry ($) . ((<$>) . (:) . TkStr *** tokens) =<< tkString s
tokens str@(c : s)
	| isSymbolChar c, let (sm, s') = (c :) `first` span isSymbolChar s =
		(TkSym sm :) <$> tokens s'
	| isDigit c, let (ds, s') = (c :) `first` span isNumChar s =
		(<$> tokens s') . (:) $ if '.' `elem` ds
			then TkDbl . read $ ds ++ "0"
			else TkInt $ read ds
	| isSpace c = tokens s
	| otherwise = Left . Error $ syntaxErr ++ tokenErr ++ show str
tokens _ = return []

tkString :: String -> Either Error (String, String)
tkString ('"' : s) = Right ("", s)
tkString ('\\' : e : s) = (<$> tkString s) . first . (:)
	$ case e of 't' -> '\t'; 'n' -> '\n'; 'r' -> '\r'; _ -> e
tkString (c : s) = first (c :) <$> tkString s
tkString _ = Left . Error $ readErr ++ strTrmErr

isSymbolChar :: Char -> Bool
isSymbolChar c = any ($ c) [isAlpha, (`elem` "+-*/<=>?")]

isNumChar :: Char -> Bool
isNumChar c = any ($ c) [isDigit, (`elem` ".")]

parse :: [Token] -> Either Error [Value]
parse [] = return []
parse ts = uncurry (<$>) . ((:) *** parse) =<< parse1 ts

parse1, parseList :: [Token] -> Either Error (Value, [Token])
parse1 (TkSym s : ts) = return (Symbol s, ts)
parse1 (TkFalse : ts) = return (Bool False, ts)
parse1 (TkTrue : ts) = return (Bool True, ts)
parse1 (TkInt i : ts) = return (Int i, ts)
parse1 (TkDbl d : ts) = return (Dbl d, ts)
parse1 (TkStr s : ts) = return (String s, ts)
parse1 (OParen : ts) = parseList ts
parse1 ts = Left . Error $ syntaxErr ++ parseErr ++ show ts

parseList (CParen : ts) = return (Nil, ts)
parseList ts = uncurry (<$>) . (first . Cons *** parseList) =<< parse1 ts
