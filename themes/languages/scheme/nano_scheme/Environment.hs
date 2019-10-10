module Environment (
	Env, M.fromList, refer, set,
	Value(..), Symbol, showValue, Error(..), ErrorMessage) where

import qualified Data.Map as M

unboundErr :: ErrorMessage
unboundErr = "*** ERROR: unbound variable: "

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Either Error Value
refer s e = maybe (Left . Error $ unboundErr ++ s) Right (M.lookup s e)

set :: Symbol -> Value -> Env -> Env
set = M.insert

data Value
	= Symbol Symbol
	| Bool Bool
	| Integer Integer
	| Double Double
	| Char Char
	| String String
	| Cons Value Value | Nil
	| Syntax Symbol (Value -> Env -> Either Error (Value, Env))
	| Subroutine Symbol (Value -> Env -> Either Error (Value, Env))
	| Lambda Symbol Value Value
	| DoExit

type Symbol = String

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Bool False) = "#f"
showValue (Bool True) = "#t"
showValue (Integer i) = show i
showValue (Double d) = show d
showValue (Char ' ') = "#\\space"
showValue (Char '\t') = "#\\tab"
showValue (Char '\n') = "#\\newline"
showValue (Char '\r') = "#\\return"
showValue (Char c) = "#\\" ++ [c]
showValue (String s) = s
showValue (Cons v vs) = '(' : showCons v vs ++ ")"
showValue Nil = "()"
showValue (Syntax n _) = "#<syntax " ++ n ++ ">"
showValue (Subroutine n _) = "#<subr " ++ n ++ ">"
showValue (Lambda n _ _) = "#<closure " ++ n ++ ">"
showValue DoExit = "#<closure exit>"

showCons :: Value -> Value -> String
showCons v Nil = showValue v
showCons v (Cons v' vs) = showValue v ++ " " ++ showCons v' vs
showCons v1 v2 = showValue v1 ++ " . " ++ showValue v2

data Error = Exit | Error ErrorMessage deriving Show

type ErrorMessage = String
