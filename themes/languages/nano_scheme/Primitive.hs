module Primitive (env0) where

import Environment (
	Env, fromList, set,
	Value(..), showValue, Error(..), ErrorMessage)

syntaxErr :: ErrorMessage
syntaxErr = "*** ERROR: Compile Error: syntax-error: "

env0 :: Env
env0 = fromList [
	("exit", DoExit),
	("define", Syntax "define" define)
	]

define :: Value -> Env -> Either Error (Value, Env)
define (Cons sm@(Symbol s) (Cons v Nil)) e = Right (sm, set s v e)
define v _ = Left . Error $ syntaxErr ++ showValue (Symbol "define" `Cons` v)
