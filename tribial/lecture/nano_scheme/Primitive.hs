module Primitive (env0) where

import Environment

env0 :: Env
env0 = fromList [
	("hoge", Int 12345),
	("+", Subr "+" add),
	("-", Subr "-" sub),
	("*", Subr "*" mul)
	]

add, sub, mul :: [Value] -> Env -> Maybe (Value, Env)
add [Int m, Int n] e = Just (Int $ m + n, e)
add _ _ = Nothing

sub [Int m, Int n] e = Just (Int $ m - n, e)
sub _ _ = Nothing

mul [Int m, Int n] e = Just (Int $ m * n, e)
mul _ _ = Nothing
