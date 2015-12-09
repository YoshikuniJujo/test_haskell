module Primitive (env0) where

import Eval
import Environment
import Maybe

env0 :: Env
env0 = fromList [
	("define", Sntx define),
	("lambda", Sntx lambda),
	("if", Sntx ifs),
	("hoge", Int 12345),
	("+", Subr . int2 $ (Int .) . (+)),
	("-", Subr . int2 $ (Int .) . (-)),
	("*", Subr . int2 $ (Int .) . (*)),
	("<", Subr . int2 $ (Bool .) . (<))
	]

define :: [Value] -> Env -> Maybe (Value, Env)
define [sm@(Sym s), v] e = (\(v', e') -> (sm, set s v' e')) `mapply` eval v e
define [List (f : ps), v] e =
	eval (List [Sym "define", f, List[Sym "lambda", List ps, v]]) e
define  _ _ = Nothing

lambda :: [Value] -> Env -> Maybe (Value, Env)
lambda [vs, v] e = (\ps -> (Lmbd ps v, e)) `mapply` symbols vs
lambda _ _ = Nothing

symbols :: Value -> Maybe [Symbol]
symbols (List vs) = ss vs
	where
	ss (Sym s : vs') = (s :) `mapply` ss vs'
	ss [] = Just []
	ss _ = Nothing
symbols _ = Nothing

bool :: (Value, Env) -> (Bool, Env)
bool (Bool False, e) = (False, e)
bool (_, e) = (True, e)

ifs :: [Value] -> Env -> Maybe (Value, Env)
ifs [p, th, el] e = (\(b, e') -> eval (if b then th else el) e')
	`mbind` (bool `mapply` eval p e)
ifs _ _ = Nothing

int2 :: (Integer -> Integer -> Value) -> [Value] -> Env -> Maybe (Value, Env)
int2 op [Int m, Int n] e = Just (m `op` n, e)
int2 _ _ _ = Nothing
