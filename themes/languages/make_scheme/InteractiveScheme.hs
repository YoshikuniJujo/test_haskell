{-# LANGUAGE TupleSections #-}

module InteractiveScheme (scheme, load, Env, env0, Error(..)) where

import Control.Applicative
import Control.Arrow
import Data.Ratio
import System.Random

import Parser (Env, Symbol, Value(..), Error(..), toDouble, parse, parseList)
import qualified Parser as P

env0 :: StdGen -> Env
env0 rg = P.fromList [
	("exit", DoExit),
	("+", Subroutine "+" . reduceL1 $ opn (+) (+)),
	("-", Subroutine "-" neg),
	("*", Subroutine "*" . reduceL1 $ opn (*) (*)),
	("/", Subroutine "/" . reduceL1 $ opn (/) (/)),
	("remainder", Subroutine "remainder" remainder),
	("=", Subroutine "=" $ equal),
	(">", Subroutine ">" $ isLargerThan),
	("<", Subroutine "<" $ isSmallerThan),
	("and", Syntax "and" andS),
	("or", Syntax "or" orS),
	("not", Subroutine "not" notS),
	("define", Syntax "define" define),
	("lambda", Syntax "lambda" lambda),
	("cond", Syntax "cond" cond),
	("if", Syntax "if" ifte),
	("random-seed", RandomSeed rg),
	("random-with", Syntax "random-with" randomWith),
	("random", Closure "random" (env0 rg) ["n"] $ Cons
		(Cons (Symbol "random-with") $
			Cons (Symbol "random-seed") $
				Cons (Symbol "n") Nil)
		Nil)
	]

randomWith :: Value -> Env -> Either Error ((String, Value), Env)
randomWith (Cons r@(Symbol s) (Cons a Nil)) e = do
	((_, rg), _) <- eval r e
	((o, v), e') <- eval a e
	output o $ randomValueTo s (Cons rg (Cons v Nil)) e'
randomWith _ _ = Left $ Error "randomValueTo: yet"

randomValueTo :: String -> Value -> Env -> Either Error ((String, Value), Env)
randomValueTo s (Cons (RandomSeed rg) (Cons (Integer n) Nil)) e = let
	(n', rg') = randomR (0, ceiling n - 1) rg in do
		e' <- P.set s (RandomSeed rg') e
		return (("", Integer $ n' % 1), e')
randomValueTo _ _ _ = Left $ Error "randomValueTo: yet"

define :: Value -> Env -> Either Error ((String, Value), Env)
define (Cons sm@(Symbol s) (Cons v Nil)) e = do
	((o, v'), e') <- eval v e
	Right ((o, sm), P.insert s v' e')
define (Cons (Cons sm@(Symbol n) as) c) e = do
	ss <- symbols as
	Right (("", sm), P.insert n (Closure n e ss c) e)
define _ _ = Left $ Error "define: error"

lambda :: Value -> Env -> Either Error ((String, Value), Env)
lambda (Cons a0 as) e = (\ss -> (("", Closure "#f" e ss as), e)) <$> symbols a0
lambda _ _ = Left $ Error "lambda: error"

symbols :: Value -> Either Error [Symbol]
symbols (Cons (Symbol s) ss) = (s :) <$> symbols ss
symbols Nil = Right []
symbols _ = Left $ Error "symbols: yet"

cond :: Value -> Env -> Either Error ((String, Value), Env)
cond Nil e = Right (("", Undef), e)
cond (Cons (Cons (Symbol "else") p) _) e = foreachC eval p e
cond (Cons (Cons t p) cs) e = do
	((to, tr), e') <- eval t e
	case tr of
		Bool False -> output to $ cond cs e'
		_ -> output to $ foreachC eval p e'
cond _ _ = Left $ Error "cond: yet"

ifte :: Value -> Env -> Either Error ((String, Value), Env)
ifte (Cons b (Cons t (Cons e Nil))) env = do
	((bo, br), env') <- eval b env
	case br of
		Bool False -> output bo $ eval e env'
		_ -> output bo $ eval t env'
ifte _ _ = Left $ Error "ifte: yet"

andS, orS, notS :: Value -> Env -> Either Error ((String, Value), Env)
andS Nil e = Right (("", Bool True), e)
andS (Cons b Nil) e = eval b e
andS (Cons b bs) e = do
	((o, br), e') <- eval b e
	case br of
		Bool False -> return ((o, br), e')
		_ -> andS bs e'
andS _ _ = Left $ Error "andS: yet"

orS Nil e = Right (("", Bool False), e)
orS (Cons b bs) e = do
	((o, br), e') <- eval b e
	case br of
		Bool False -> orS bs e'
		_ -> return ((o, br), e')
orS _ _ = Left $ Error "orS: yet"

notS (Cons b Nil) e = do
	((o, br), e') <- eval b e
	return $ case br of
		Bool False -> ((o, Bool True), e')
		_ -> ((o, Bool False), e')
notS _ _ = Left $ Error "notS: yet"

output :: String -> Either Error ((String, Value), Env) ->
	Either Error ((String, Value), Env)
output o r = (((o ++) `first`) `first`) <$> r

reduceL1 :: (Value -> Value -> Env -> Either Error ((String, Value), Env)) ->
	Value -> Env -> Either Error ((String, Value), Env)
reduceL1 op (Cons v0 vs) e = reduceL op v0 vs e
reduceL1 _ _ _ = Left $ Error "reduceL1: yet"

reduceL :: (Value -> Value -> Env -> Either Error ((String, Value), Env)) ->
	Value -> Value -> Env -> Either Error ((String, Value), Env)
reduceL op v0 (Cons v vs) e = case op v0 v e of
	Right ((o, v'), e') -> output o $ reduceL op v' vs e'
	er -> er
reduceL _ v0 Nil e = Right (("", v0), e)
reduceL _ _ _ _ = Left $ Error "reduceL: yet"

opn :: (Rational -> Rational -> Rational) -> (Double -> Double -> Double) ->
	Value -> Value -> Env -> Either Error ((String, Value), Env)
opn opi _ (Integer n1) (Integer n2) e =
	Right . (, e) . ("" ,) . Integer $ n1 `opi` n2
opn _ opd v1 v2 e = case (toDouble v1, toDouble v2) of
	(Just d1, Just d2) -> Right . (, e) . ("" ,) . Double $ d1 `opd` d2
	_ -> Left . Error $ "operation ... is not defined between " ++
		toStr v1 ++ " " ++ toStr v2

remainder :: Value -> Env -> Either Error ((String, Value), Env)
remainder (Cons (Integer n1) (Cons (Integer n2) Nil)) e
	| 1 <- denominator n1, 1 <- denominator n2 =
		Right (("", Integer $ numerator n1 `mod` numerator n2 % 1),
			e)
remainder _ _ = Left . Error $ "remainder: yet"

neg :: Value -> Env -> Either Error ((String, Value), Env)
neg (Cons (Integer n) Nil) e = Right (("", Integer $ - n), e)
neg (Cons (Double n) Nil) e = Right (("", Double $ - n), e)
neg v e = reduceL1 (opn (-) (-)) v e

equal, isLargerThan, isSmallerThan ::
	Value -> Env -> Either Error ((String, Value), Env)
equal (Cons (Integer n1) (Cons (Integer n2) Nil)) e =
	Right (("", Bool $ n1 == n2), e)
equal v _ = Left . Error $ "equal: yet: " ++ show v

isLargerThan (Cons (Integer n1) (Cons (Integer n2) Nil)) e =
	Right (("", Bool $ n1 > n2), e)
isLargerThan (Cons v1 (Cons v2 Nil)) e = case (toDouble v1, toDouble v2) of
	(Just d1, Just d2) -> Right (("", Bool $ d1 > d2), e)
	_ -> Left . Error $ "isLargerThan: yet"
isLargerThan _ _ = Left . Error $ "isLargerThan: yet"

isSmallerThan (Cons (Integer n1) (Cons (Integer n2) Nil)) e =
	Right (("", Bool $ n1 < n2), e)
isSmallerThan (Cons v1 (Cons v2 Nil)) e = case (toDouble v1, toDouble v2) of
	(Just d1, Just d2) -> Right (("", Bool $ d1 < d2), e)
	_ -> Left . Error $ "isSmallerThan: yet"
isSmallerThan _ _ = Left . Error $ "isSmallerThan: yet"

load :: String -> Env -> Either Error (String, Env)
load = loadV . fst . parseList

loadV :: Value -> Env -> Either Error (String, Env)
loadV Nil e = Right ("", e)
loadV (Cons v vs) e = do
	((o, _), e') <- eval v e
	first (o ++) <$> loadV vs e'
loadV _ _ = Left $ Error "loadV: yet"

scheme :: String -> Env -> Either Error (String, Env)
scheme s e = first (uncurry (++) . second toStr) <$>
	((`eval` e) . fst =<< parse s)

toStr :: Value -> String
toStr Undef = "#<undef>"
toStr (Bool False) = "#f"
toStr (Bool True) = "#t"
toStr (Symbol s) = s
toStr (Integer i) = case (numerator i, denominator i) of
	(n, 1) -> show n
	(n, d) -> show n ++ "/" ++ show d
toStr (Double d) = show d
toStr (Cons v Nil) = '(' : toStr v ++ ")"
toStr (Cons v _) = '(' : toStr v ++ " ..)"
toStr Nil = "()"
toStr DoExit = "#<closure exit>"
toStr (Subroutine n _) = "#<subr " ++ n ++ ">"
toStr (Syntax n _) = "#<syntax " ++ n ++ ">"
toStr (Closure n _ _ _) = "#<closure " ++ n ++ ">"
toStr (RandomSeed rg) = "#<random-seed " ++ show rg ++ ">"
toStr _ = error "toStr: yet"

eval :: Value -> Env -> Either Error ((String, Value), Env)
eval (Symbol s) e = case P.lookup s e of
	Just v -> Right (("", v), e)
	_ -> Left . Error $ "*** ERROR: unbound variable: " ++ s
eval (Cons v1 v2) e = do
	((o1, p), e') <- eval v1 e
	first (first (o1 ++)) <$> apply p v2 e'
eval v e = Right (("", v), e)

mapC :: (Value -> Env -> Either Error ((String, Value), Env)) -> Value -> Env ->
	Either Error ((String, Value), Env)
mapC f (Cons v vs) e = do
	((o, v'), e') <- f v e
	first ((o ++) *** Cons v') <$> mapC f vs e'
mapC _ Nil e = Right (("", Nil), e)
mapC _ v _ = Left . Error $ "*** ERROR: Compile Error: proper list required for " ++
	"function application or macro use: " ++ toStr v

foreachC :: (Value -> Env -> Either Error ((String, Value), Env)) -> Value -> Env ->
	Either Error ((String, Value), Env)
foreachC f (Cons v Nil) e = f v e
foreachC f (Cons v vs) e = do
	((o, _), e') <- f v e
	first ((o ++) `first`) <$> foreachC f vs e'
foreachC _ Nil e = Right (("", Nil), e)
foreachC _ v _ = Left . Error $ "*** ERROR: Compile Error: proper list required for " ++
	"function application or macro use: " ++ toStr v

apply :: Value -> Value -> Env -> Either Error ((String, Value), Env)
apply DoExit Nil _ = Left Exit
apply (Syntax _ s) v e = s v e
apply (Subroutine _ sr) v e = do
	((o2, as), e'') <- mapC eval v e
	first (first (o2 ++)) <$> sr as e''
apply (Closure _ _ ss c) v e0 = do
	((o2, as), e'') <- mapC eval v e0
	first (first (o2 ++)) . second P.exit
		<$> (foreachC eval c =<< defineAll ss as (P.local e''))
apply f as _ = Left . Error $ "apply: yet: " ++ show f ++ " " ++ show as

defineAll :: [Symbol] -> Value -> Env -> Either Error Env
defineAll [] Nil e = Right e
defineAll (s : ss) (Cons v vs) e = P.insert s v <$> defineAll ss vs e
defineAll _ _ _ = Left $ Error "defineAll: yet"
