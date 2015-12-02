{-# LANGUAGE TupleSections #-}

module Primitive (env0) where

import Control.Applicative ((<$>))

import Environment (
	Env, fromList, set,
	Value(..), showValue, Error(..), ErrorMessage)

syntaxErr, prpLstErr, notNumErr :: ErrorMessage
syntaxErr = "*** ERROR: Compile Error: syntax-error: "
prpLstErr = "*** ERROR: Compile Error: proper list required: "
notNumErr = "*** ERROR: Not Number: "

env0 :: Env
env0 = fromList [
	("exit", DoExit),
	("define", Syntax "define" define),
	("lambda", Syntax "lambda" lambda),
	("list", Subroutine "list" $ (Right .) . (,)),
	("+", Subroutine "+" add)
	]

define, lambda :: Value -> Env -> Either Error (Value, Env)
define (Cons sm@(Symbol s) (Cons v Nil)) e = Right (sm, set s v e)
define v _ = Left . Error $ syntaxErr ++ showValue (Symbol "define" `Cons` v)

lambda (Cons as bd) e = Right (Lambda "#f" as bd, e)
lambda v _ = Left . Error $ syntaxErr ++ showValue (Symbol "lambda" `Cons` v)

consToList :: Value -> Either Error [Value]
consToList Nil = Right []
consToList (v `Cons` vs) = (v :) <$> consToList vs
consToList v = Left . Error $ prpLstErr ++ showValue v

toInt :: Value -> Either Error Integer
toInt (Integer i) = Right i
toInt v = Left . Error $ notNumErr ++ showValue v

add :: Value -> Env -> Either Error (Value, Env)
add v e = (, e) . Integer . sum <$> (mapM toInt =<< consToList v)
