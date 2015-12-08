module Primitive (env0) where

import Eval
import Environment
import Maybe

env0 :: Env
env0 = fromList [
	("define", Sntx "define" define),
	("lambda", Sntx "lambda" lambda),
	("hoge", Int 12345),
	("+", Subr "+" add),
	("-", Subr "-" sub),
	("*", Subr "*" mul)
	]

define :: [Value] -> Env -> Maybe (Value, Env)
define [sm@(Symbol s), v] e = (\(v', e') -> (sm, set s v' e')) `mapply` eval v e
define [List (sm@(Symbol _) : ps), v] e = (`eval` e) $ List [
	Symbol "define", sm, List [Symbol "lambda", List ps, v]]
define _ _ = Nothing

lambda :: [Value] -> Env -> Maybe (Value, Env)
lambda [ps, v] e = (\as -> (Lmbd as v, e)) `mapply` symbols ps
lambda _ _ = Nothing

symbols :: Value -> Maybe [Symbol]
symbols (List vs) = ss vs
	where
	ss (Symbol s : vs') = (s :) `mapply` ss vs'
	ss [] = Just []
	ss _ = Nothing
symbols _ = Nothing

add, sub, mul :: [Value] -> Env -> Maybe (Value, Env)
add [Int m, Int n] e = Just (Int $ m + n, e)
add _ _ = Nothing

sub [Int m, Int n] e = Just (Int $ m - n, e)
sub _ _ = Nothing

mul [Int m, Int n] e = Just (Int $ m * n, e)
mul _ _ = Nothing
