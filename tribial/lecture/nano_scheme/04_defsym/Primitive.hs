module Primitive (env0) where

import Eval
import Environment
import Maybe

env0 :: Env
env0 = fromList [
	("define", Sntx define),
	("hoge", Int 12345),
	("+", Subr . int2 $ (Int .) . (+)),
	("-", Subr . int2 $ (Int .) . (-)),
	("*", Subr . int2 $ (Int .) . (*))
	]

define :: [Value] -> Env -> Maybe (Value, Env)
define [sm@(Sym s), v] e = (\(v', e') -> (sm, set s v' e')) `mapply` eval v e
define  _ _ = Nothing

int2 :: (Integer -> Integer -> Value) -> [Value] -> Env -> Maybe (Value, Env)
int2 op [Int m, Int n] e = Just (m `op` n, e)
int2 _ _ _ = Nothing
