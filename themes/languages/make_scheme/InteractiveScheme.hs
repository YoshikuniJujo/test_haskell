module InteractiveScheme (scheme, Env, env0, Error(..)) where

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

import Parser

type Env = M.Map Symbol Value

env0 :: Env
env0 = M.fromList [
	("exit", DoExit)
	]

scheme :: String -> Env -> Either Error (String, Env)
scheme s e = first (uncurry (++) . (second toStr)) <$>
	((`eval` e) . fst =<< parse s)

toStr :: Value -> String
toStr Undef = "#<undef>"
toStr (Symbol s) = s
toStr (Integer i) = show i
toStr (Cons v Nil) = '(' : toStr v ++ ")"
toStr DoExit = "#<closure exit>"
toStr _ = error "toStr: yet"

eval :: Value -> Env -> Either Error ((String, Value), Env)
eval (Symbol s) e = case M.lookup s e of
	Just v -> Right (("", v), e)
	_ -> Left . Error $ "*** ERROR: unbound variable: " ++ s
eval (Cons v1 v2) e = do
	((o1, p), e') <- eval v1 e
	((o2, as), e'') <- mapC eval v2 e'
	first (first ((o1 ++ o2) ++)) <$> apply p as e''
eval v e = Right (("", v), e)

mapC :: (Value -> Env -> Either Error ((String, Value), Env)) -> Value -> Env ->
	Either Error ((String, Value), Env)
mapC f (Cons v vs) e = do
	((o, v'), e') <- f v e
	first ((o ++) *** Cons v') <$> mapC f vs e'
mapC _ Nil e = Right (("", Nil), e)
mapC _ v _ = Left . Error $ "*** ERROR: Compile Error: proper list required for " ++
	"function application or macro use: " ++ toStr v

apply :: Value -> Value -> Env -> Either Error ((String, Value), Env)
apply DoExit Nil _ = Left Exit
apply _ _ _ = Left (Error "apply: yet")
