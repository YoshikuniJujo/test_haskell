module Parser (
	parse, parseList,
	Value(..), Symbol, Error(..), toDouble,
	Env, E.lookup, E.insert, E.set, E.fromList, E.local, E.exit,
	E.getScope, E.setScope, E.global,
	) where

import Control.Arrow
import Data.Char

import Environment (
	Value(..), toDouble, negateValue, Error(..), Env, Symbol)
import qualified Environment as E

parse :: String -> Either Error (Value, String)
parse ('(' : s) = let (c, s') = parseList s in case dropWhile isSpace s' of
	')' : s'' -> Right (c, s'')
	_ -> Left $ Error "READ-ERROR"
parse ('#' : c : s) = case c of
	'f' -> Right $ (Bool False, s)
	't' -> Right $ (Bool True, s)
	_ -> Left . Error $ "parse error: #" ++ [c]
parse ('-' : c : s) | isDigit c = Right
	. (negateValue `first`)
	. uncurry parseNumber . first (c :) $ span isDigit s
parse ('"' : s) = Right . (String *** tail) $ span (/= '"') s
parse (c : s)
	| isDigit c =
		Right . uncurry parseNumber . first (c :) $ span isDigit s
	| isSymbolHeadChar c = Right .
		((Symbol . (c :)) `first`) $ span isSymbolChar s
	| isSpace c = parse s
parse _ = Left $ Error "parse error"

parseNumber :: String -> String -> (Value, String)
parseNumber ds ('.' : s) = (Double . read $ ds ++ "." ++ dcs, s')
	where (dcs, s') = span isDigit s
parseNumber ds s = (Integer . fromIntegral $ (read ds :: Integer), s)

parseList :: String -> (Value, String)
parseList s = case parse s of
	Right (v, r) -> (v `Cons`) `first` parseList r
	_ -> (Nil, s)

isSymbolHeadChar, isSymbolChar :: Char -> Bool
isSymbolHeadChar c = any ($ c) [isAlpha, (`elem` "+-*/<=>?")]
isSymbolChar c = any ($ c) [isDigit, isSymbolHeadChar]
