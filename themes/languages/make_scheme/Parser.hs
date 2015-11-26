module Parser (parse, Value(..), toDouble, Env, Symbol, Error(..)) where

import Control.Arrow
import qualified Data.Map as M
import Data.Ratio
import Data.Char

type Env = M.Map Symbol Value

data Value
	= Undef | Nil | Bool Bool | Symbol Symbol
	| Integer Rational | Double Double | String String
	| Cons Value Value
	| DoExit | Display String
	| Subroutine String (Value -> Env -> Either Error ((String, Value), Env))
	| Syntax String (Value -> Env -> Either Error ((String, Value), Env))
	| Closure [Symbol] Value
	| Define Symbol Value

toDouble :: Value -> Maybe Double
toDouble (Integer r) = Just $ realToFrac r
toDouble (Double d) = Just d
toDouble _ = Nothing

-- apply :: Value -> Value -> Env -> Either Error ((String, Value), Env)

instance Show Value where
	show (Symbol s) = s
	show (Integer i) = show i
	show (Double d) = show d
	show (Cons v1 v2) = "(" ++ show v1 ++ " . " ++ show v2 ++ ")"
	show Nil = "()"
	show _ = "instance Show Value: yet"

type Symbol = String

data Error
	= Exit
	| Error String
	deriving Show

parse :: String -> Either Error (Value, String)
parse ('(' : s) = let (c, s') = parseList s in case dropWhile isSpace s' of
	')' : s'' -> Right (c, s'')
	_ -> Left $ Error "READ-ERROR"
parse (c : s)
	| isDigit c =
		Right . uncurry parseNumber . first (c :) $ span isDigit s
	| isSymbolChar c = Right .
		((Symbol . (c :)) `first`) $ span isSymbolChar s
	| isSpace c = parse s
parse _ = Left $ Error "parse error"

parseNumber :: String -> String -> (Value, String)
parseNumber ds ('.' : s) = (Double . read $ ds ++ "." ++ dcs, s')
	where (dcs, s') = span isDigit s
parseNumber ds s = (Integer . fromIntegral $ read ds, s)

parseList :: String -> (Value, String)
parseList s = case parse s of
	Right (v, r) -> (v `Cons`) `first` parseList r
	_ -> (Nil, s)

isSymbolChar :: Char -> Bool
isSymbolChar c = any ($ c) [isAlpha, (`elem` "+-*/")]
