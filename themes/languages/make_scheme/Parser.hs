module Parser (parse, Value(..), Env, Symbol, Error(..)) where

import Control.Arrow
import qualified Data.Map as M
import Data.Ratio
import Data.Char

type Env = M.Map Symbol Value

data Value
	= Undef | Nil | Bool Bool | Symbol Symbol
	| Integer Rational | String String
	| Cons Value Value
	| DoExit | Display String
	| Subroutine (Value -> Env -> Either Error ((String, Value), Env))
	| Closure [Symbol] Value
	| Define Symbol Value

-- apply :: Value -> Value -> Env -> Either Error ((String, Value), Env)

instance Show Value where
	show (Integer i) = show i
	show (Cons v1 v2) = "(" ++ show v1 ++ " . " ++ show v2 ++ ")"
	show Nil = "()"
	show _ = "instance Show Value: yet"

type Symbol = String

data Error
	= Exit
	| Error String
	deriving Show

parse :: String -> Either Error (Value, String)
parse ('(' : s) = Right $ parseList s
--	(Cons (Symbol $ takeWhile (/= ')') s) Nil, dropWhile (/= ')') s)
parse (c : s)
	| isDigit c = Right $ (
		Integer . fromIntegral . read $ c : takeWhile isDigit s,
		dropWhile isDigit s)
	| isSymbolChar c = Right .
		((Symbol . (c :)) `first`) $ span isSymbolChar s
	| isSpace c = parse s
parse _ = Left $ Error "parse error"

parseList :: String -> (Value, String)
parseList s = case parse s of
	Right (v, r) -> (v `Cons`) `first` parseList r
	_ -> (Nil, s)

isSymbolChar :: Char -> Bool
isSymbolChar c = any ($ c) [isAlpha, (`elem` "+-*/")]
