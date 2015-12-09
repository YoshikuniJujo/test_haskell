module Primitive (env0) where

import Environment

env0 :: Env
env0 = fromList [
	("hoge", Int 12345),
	("+", Subr . int2 $ (Int .) . (+)),
	("-", Subr . int2 $ (Int .) . (-)),
	("*", Subr . int2 $ (Int .) . (*))
	]

int2 :: (Integer -> Integer -> Value) -> [Value] -> Env -> Maybe (Value, Env)
int2 op [Int m, Int n] e = Just (m `op` n, e)
int2 _ _ _ = Nothing
