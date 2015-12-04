module Environment (
	Env, M.fromList, refer, set, Value(..), Symbol, showValue) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Maybe Value
refer = M.lookup

set :: Symbol -> Value -> Env -> Env
set = M.insert

data Value
	= Symbol Symbol | B Bool | Int Integer | Cons Value Value | Nil
	| Syntax Symbol (Value -> Env -> Maybe (Value, Env))
	| Subroutine Symbol (Value -> Env -> Maybe (Value, Env))
	| Lambda Symbol Value Value

type Symbol = String

showValue :: Value -> String
showValue (Symbol s) = s
showValue (B False) = "#f"
showValue (B True) = "#t"
showValue (Int i) = show i
showValue (Cons v vs) = '(' : showCons v vs ++ ")"
showValue Nil = "()"
showValue (Syntax n _) = "#<syntax " ++ n ++ ">"
showValue (Subroutine n _) = "#<subr " ++ n ++ ">"
showValue (Lambda n _ _) = "#<closure " ++ n ++ ">"

showCons :: Value -> Value -> String
showCons v Nil = showValue v
showCons v1 (Cons v2 vs) = showValue v1 ++ " " ++ showCons v2 vs
showCons v1 v2 = showValue v1 ++ " . " ++ showValue v2
