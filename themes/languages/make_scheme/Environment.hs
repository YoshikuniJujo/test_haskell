module Environment (
	Symbol, Value(..), Error(..), toDouble,
	Env, Environment.lookup, Environment.insert, Environment.fromList,
	) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

lookup :: Symbol -> Env -> Maybe Value
lookup = M.lookup

insert :: Symbol -> Value -> Env -> Env
insert = M.insert

fromList :: [(Symbol, Value)] -> Env
fromList = M.fromList

type Symbol = String

data Value
	= Undef | Nil | Bool Bool | Symbol Symbol
	| Integer Rational | Double Double | String String
	| Cons Value Value
	| DoExit | Display String
	| Subroutine Symbol (Value -> Env -> Either Error ((String, Value), Env))
	| Syntax Symbol (Value -> Env -> Either Error ((String, Value), Env))
	| Closure Symbol Env [Symbol] Value
	| Define Symbol Value

instance Show Value where
	show (Symbol s) = s
	show (Integer i) = show i
	show (Double d) = show d
	show (Cons v1 v2) = "(" ++ show v1 ++ " . " ++ show v2 ++ ")"
	show Nil = "()"
	show _ = "instance Show Value: yet"

data Error
	= Exit
	| Error String
	deriving Show

toDouble :: Value -> Maybe Double
toDouble (Integer r) = Just $ realToFrac r
toDouble (Double d) = Just d
toDouble _ = Nothing
