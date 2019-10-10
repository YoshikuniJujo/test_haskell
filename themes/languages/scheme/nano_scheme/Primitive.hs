{-# LANGUAGE TupleSections #-}

module Primitive (env0) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')
import Data.Char (ord, chr, toUpper, toLower)

import Eval (eval)
import Environment (
	Env, fromList, set,
	Value(..), showValue, Error(..), ErrorMessage)

syntaxErr, prpLstErr, notNumErr, lstOneErr,
	chrReqErr, strReqErr, intReqErr, lstReqErr, noApplErr
	:: ErrorMessage
syntaxErr = "*** ERROR: Compile Error: syntax-error: "
prpLstErr = "*** ERROR: Compile Error: proper list required: "
notNumErr = "*** ERROR: Not Number: "
lstOneErr = "*** ERROR: Compile Error: procedure requires at least one argument"
chrReqErr = "*** ERROR: character required, but got "
strReqErr = "*** ERROR: string required, but got "
intReqErr = "*** ERROR: integer required, but got "
lstReqErr = "*** ERROR: list required, but got "
noApplErr = "*** ERROR: no applicable method: "

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
	("<", Subroutine "<" ltt),
	("char=?", Subroutine "char=?" charEq),
	("char->integer", Subroutine "char->integer" char2integer),
	("integer->char", Subroutine "integer->char" integer2char),
	("char-upcase", Subroutine "char-upcase" charUpcase),
	("char-downcase", Subroutine "char-downcase" charDowncase),
	("string-append", Subroutine "string-append" stringAppend),
	("string->list", Subroutine "string->list" string2list),
	("list->string", Subroutine "list->string" list2string),
	("x->string", Subroutine "x->string" x2string)
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

char2integer, integer2char :: Value -> Env -> Either Error (Value, Env)
char2integer (Cons (Char c) Nil) e = Right (Integer . fromIntegral $ ord c, e)
char2integer v _ = Left . Error $ chrReqErr ++ showValue v

integer2char (Cons (Integer i) Nil) e = Right (Char . chr $ fromIntegral i, e)
integer2char v _ = Left . Error $ intReqErr ++ showValue v

charUpcase, charDowncase :: Value -> Env -> Either Error (Value, Env)
charUpcase (Cons (Char c) Nil) e = Right (Char $ toUpper c, e)
charUpcase v _ = Left . Error $ chrReqErr ++ showValue v

charDowncase (Cons (Char c) Nil) e = Right (Char $ toLower c, e)
charDowncase v _ = Left . Error $ chrReqErr ++ showValue v

stringAppend :: Value -> Env -> Either Error (Value, Env)
stringAppend (Cons (String str) strs) e = first (sa str) <$> stringAppend strs e
	where
	sa s (String ss) = String $ s ++ ss
	sa _ _ = error "never occur"
stringAppend Nil e = Right (String "", e)
stringAppend v _ = Left . Error $ strReqErr ++ showValue v

string2list, list2string :: Value -> Env -> Either Error (Value, Env)
string2list (Cons (String str) Nil) e = Right (s2l str, e)
	where s2l (c : cs) = Char c `Cons` s2l cs; s2l _ = Nil
string2list v _ = Left . Error $ strReqErr ++ showValue v

list2string (Cons val Nil) e = (, e) . String <$> l2s val
	where
	l2s (Cons (Char c) cs) = (c :) <$> l2s cs
	l2s Nil = Right ""
	l2s v = Left . Error $ chrReqErr ++ showValue v
list2string v _ = Left . Error $ lstReqErr ++ showValue v

x2string :: Value -> Env -> Either Error (Value, Env)
x2string (Cons v Nil) e = Right (String $ showValue v, e)
x2string v _ = Left . Error $ noApplErr ++ showValue v

charEq :: Value -> Env -> Either Error (Value, Env)
charEq v e = (, e) . Bool . and . (zipWith (==) <$> id <*> tail)
	<$> (mapM toChar =<< consToList v)

toChar :: Value -> Either Error Char
toChar (Char c) = Right c
toChar v = Left . Error $ chrReqErr ++ showValue v
