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
	as__ <- cdr "*** ERROR" o
	as_ <- case as__ of
		v@(OVar _) -> eval v
		_ -> return as__
	case f of
		OSubr _ s -> s =<< mapCons eval as_
		OSyntax _ s -> s as_
		OClosure _ eid ps bd -> do
			as <- mapCons eval as_
			apply eid ps as bd
		o' -> throwError $ "eval: bad in function: " ++ showObj o'

apply :: EID -> Object -> Object -> Object -> SchemeM Object
apply eid ps as bd = do
	newEnv eid
	defArgs ps as
	r <- lastCons =<< mapCons eval bd
	popEnv
	return r

defArgs :: Object -> Object -> SchemeM ()
defArgs ONil ONil = return ()
defArgs (OVar var) val = define var val
defArgs vara vala = do
	(OVar var) <- car "defArgs" vara
	val <- car "defArgs" vala
	vars <- cdr "defArgs" vara
	vals <- cdr "defArgs" vala
	define var val >> defArgs vars vals
