module Primitive (env0) where

import Eval
import Environment
import Maybe

env0 :: Env
env0 = fromList [
	("define", Sntx "define" define),
	("hoge", Int 12345),
	("+", Subr "+" add),
	("-", Subr "-" sub),
	("*", Subr "*" mul)
	]

define :: [Value] -> Env -> Maybe (Value, Env)
define [sm@(Symbol s), v] e = (\(v', e') -> (sm, set s v' e')) `mapply` eval v e
define _ _ = Nothing

add, sub, mul :: [Value] -> Env -> Maybe (Value, Env)
add [Int m, Int n] e = Just (Int $ m + n, e)
add _ _ = Nothing

sub [Int m, Int n] e = Just (Int $ m - n, e)
sub _ _ = Nothing

mul [Int m, Int n] e = Just (Int $ m * n, e)
mul _ _ = Nothing
