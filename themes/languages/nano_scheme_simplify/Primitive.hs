{-# LANGUAGE TupleSections #-}

module Primitive (env0) where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')

import Eval (eval)
import Environment (Env, fromList, set, Value(..))

env0 :: Env
env0 = fromList [
	("define", Syntax "define" define), ("lambda", Syntax "lambda" lambda),
	("if", Syntax "if" ifs),
	("+", Subroutine "+" add), ("-", Subroutine "-" sub),
	("*", Subroutine "*" mul), ("<", Subroutine "<" ltt) ]

define, lambda, ifs :: Value -> Env -> Maybe (Value, Env)
define (Cons a@(Symbol s) (Cons v Nil)) e = (a ,) . uncurry (set s) <$> eval v e
define (Cons (Cons f@(Symbol n) as) bd) e = Just (f, set n (Lambda n as bd) e)
define _ _ = Nothing

lambda (Cons as bd) e = Just (Lambda "#f" as bd, e)
lambda _ _ = Nothing

ifs (Cons p (Cons th (Cons el Nil))) e = eval p e >>= \(b, e') ->
	case b of B False -> eval el e'; _ -> eval th e'
ifs _ _ = Nothing

nums :: Value -> Maybe [Integer]
nums Nil = Just []
nums (Int i `Cons` vs) = (i :) <$> nums vs
nums _ = Nothing

add, mul, sub, ltt :: Value -> Env -> Maybe (Value, Env)
add v e = (, e) . Int . sum <$> nums v
mul v e = (, e) . Int . product <$> nums v
sub v e = (, e) . Int . sb <$> nums v
	where sb [x] = - x; sb (x : xs) = foldl' (-) x xs; sb _ = 0
ltt v e = (, e) . B . and . (zipWith (<) <$> id <*> tail) <$> nums v
