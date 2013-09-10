module Eval (
	eval,
	Environment, mkInitEnv,
	EnvT, runEnvT, runSchemeM,
	Object(..), showObj,
	throwError, catchError,
	foldlCons,
	SchemeM,
	define, getEID,
	lastCons, mapCons, zipWithCons,

	cons,
	car, cdr,
	cons2list,
) where

import Object
-- import Control.Applicative

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval d@(ODouble _) = return d
eval r@(ORational _) =  return r
eval s@(OString _) = return s
eval (OVar v) = getValue v
eval n@ONil = return n
eval u@OUndef = return u
eval e@OError = return e
eval b@(OBool _) = return b
eval s@(OSubr _ _) = return s
eval s@(OSyntax _ _) = return s
eval c@(OClosure _ _ _ _) = return c
eval o = do
	f <- eval =<< car "*** ERROR" o
	as_ <- cdr "*** ERROR" o
	case f of
		OSubr _ s -> s =<< mapCons eval as_
		OSyntax _ s -> s as_
		OClosure _ eid ps bd -> do
			as <- mapCons eval as_
			apply eid ps as bd
		o' -> throwError $ "eval: bad in function: " ++ showObj o'

def :: Object -> Object -> SchemeM Object
def v@(OVar var) val = define var val >> return v
def v val = throwError $ "def: bad: " ++ showObj v ++ " " ++ showObj val

apply :: EID -> Object -> Object -> Object -> SchemeM Object
apply eid ps as bd = do
	newEnv eid
	_ <- zipWithCons def ps as
	r <- lastCons =<< mapCons eval bd
	popEnv
	return r
