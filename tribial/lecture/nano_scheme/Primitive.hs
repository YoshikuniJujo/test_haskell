module Primitive (env0) where

import Environment

env0 :: Env
env0 = fromList [
	("hoge", Int 12345),
	("+", Subr "+" add)
	]

add :: [Value] -> Env -> Maybe (Value, Env)
add [Int m, Int n] e = Just (Int $ m + n, e)
add _ _ = Nothing
