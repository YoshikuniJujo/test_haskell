module Environment (
	Env, M.fromList, refer, set,
	Value(..), Symbol, showValue, module ErrorMessage) where

import qualified Data.Map as M
import ErrorMessage

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Either ErrMsg Value
refer s e = maybe (Left $ unboundErr ++ s) Right (M.lookup s e)

set :: Symbol -> Value -> Env -> Env
set = M.insert

data Value
	= Symbol Symbol | Bool Bool | Int Integer | Dbl Double | Str String
	| Cons Value Value | Nil
	| Syntax Symbol (Value -> Env -> Either ErrMsg (Value, Env))
	| Subroutine Symbol (Value -> Env -> Either ErrMsg (Value, Env))
	| Lambda Symbol Value Value

type Symbol = String

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Bool False) = "#f"
showValue (Bool True) = "#t"
showValue (Int i) = show i
showValue (Dbl d) = show d
showValue (Str s) = s
showValue (Cons v vs) = '(' : showCons v vs ++ ")"
showValue Nil = "()"
showValue (Syntax n _) = "#<syntax " ++ n ++ ">"
showValue (Subroutine n _) = "#<subr " ++ n ++ ">"
showValue (Lambda n _ _) = "#<closure " ++ n ++ ">"

showCons :: Value -> Value -> String
showCons v Nil = showValue v
showCons v (Cons v' vs) = showValue v ++ " " ++ showCons v' vs
showCons v1 v2 = showValue v1 ++ " . " ++ showValue v2
