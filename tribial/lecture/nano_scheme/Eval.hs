{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Environment
import Maybe

eval :: Value -> Env -> Maybe (Value, Env)
eval (Symbol s) e = (, e) `mapply` refer s e
eval i@(Int _) e = Just (i, e)
eval l@(List _) e = Just (l, e)
