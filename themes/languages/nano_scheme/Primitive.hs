{-# LANGUAGE TupleSections #-}

module Primitive (env0) where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')

import Eval (eval)
import Environment (
	Env, fromList, set,
	Value(..), showValue, Error(..), ErrorMessage)

syntaxErr, prpLstErr, notNumErr, lstOneErr :: ErrorMessage
syntaxErr = "*** ERROR: Compile Error: syntax-error: "
prpLstErr = "*** ERROR: Compile Error: proper list required: "
notNumErr = "*** ERROR: Not Number: "
lstOneErr = "*** ERROR: Compile Error: procedure requires at least one argument"

env0 :: Env
env0 = fromList [
	("exit", DoExit),
	("define", Syntax "define" define),
	("lambda", Syntax "lambda" lambda),
	("if", Syntax "if" ifs),
	("list", Subroutine "list" $ (Right .) . (,)),
	("+", Subroutine "+" add),
	("*", Subroutine "*" mul),
	("-", Subroutine "-" sub),
	("/", Subroutine "/" divs),
	("<", Subroutine "<" ltt)
	]

define, lambda, ifs :: Value -> Env -> Either Error (Value, Env)
define (Cons sm@(Symbol s) (Cons v Nil)) e = do
	(v', e') <- eval v e
	Right (sm, set s v' e')
define (Cons (Cons f@(Symbol n) as) bd) e = Right (f, set n (Lambda n as bd) e)
define v _ = Left . Error $ syntaxErr ++ showValue (Symbol "define" `Cons` v)

lambda (Cons as bd) e = Right (Lambda "#f" as bd, e)
lambda v _ = Left . Error $ syntaxErr ++ showValue (Symbol "lambda" `Cons` v)

ifs (Cons p (Cons th (Cons el Nil))) e = eval p e >>= \(b, e') ->
	case b of Bool False -> eval el e'; _ -> eval th e'
ifs v _ = Left . Error $ syntaxErr ++ showValue (Symbol "if" `Cons` v)

consToList :: Value -> Either Error [Value]
consToList Nil = Right []
consToList (v `Cons` vs) = (v :) <$> consToList vs
consToList v = Left . Error $ prpLstErr ++ showValue v

toNums :: [Value] -> Either Error (Either [Integer] [Double])
toNums [] = Right $ Left []
toNums (Integer i : vs) =
	either (Left . (i :)) (Right . (fromIntegral i :)) <$> toNums vs
toNums (Double d : vs) =
	either (Right . (d :) . map fromIntegral) (Right . (d :)) <$> toNums vs
toNums (v : _) = Left . Error $ notNumErr ++ showValue v

add, mul, sub :: Value -> Env -> Either Error (Value, Env)
add v e = (, e) . either (Integer . sum) (Double . sum)
	<$> (toNums =<< consToList v)
mul v e = (, e) . either (Integer . product) (Double . product)
	<$> (toNums =<< consToList v)
sub v e = (, e) . either (Integer . sb) (Double . sb)
	<$> (chk =<< toNums =<< consToList v)
	where
	chk vs	| either null null vs = Left $ Error lstOneErr
		| otherwise = Right vs
	sb [x] = negate x
	sb (x : xs) = foldl' (-) x xs
	sb _ = error "never occur"

divs :: Value -> Env -> Either Error (Value, Env)
divs v e = (, e) . Double . dv . either (map fromIntegral) id
	<$> (chk =<< toNums =<< consToList v)
	where
	chk vs	| either null null vs = Left $ Error lstOneErr
		| otherwise = Right vs
	dv [x] = recip x
	dv (x : xs) = foldl' (/) x xs
	dv _ = error "never occur"

ltt :: Value -> Env -> Either Error (Value, Env)
ltt v e = (, e) . Bool . and
	. either (zipWith (<) <$> id <*> tail) (zipWith (<) <$> id <*> tail)
	<$> (toNums =<< consToList v)
