module Eval (
	eval,
	Environment, fromList,
	EnvT, runEnvT,
	Object(..), showObj,
	throwError, catchError,
	foldlCons,
	SchemeM,
	define, getEID,
	lastCons, mapCons, zipWithCons,
) where

import Object

import Control.Applicative

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval d@(ODouble _) = return d
eval r@(ORational _) =  return r
eval (OVar v) = getValue v
eval (OCons f_ as_) = do
	f <- eval f_
	case f of
		OSubr _ s -> s =<< mapCons eval as_
		OSyntax _ s -> s as_
		OClosure _ eid ps bd -> do
			as <- mapCons eval as_
			apply eid ps as bd
		o -> error $ "eval: bad in function: " ++ showObj o
eval n@ONil = return n
eval u@OUndef = return u
eval b@(OBool _) = return b
eval s@(OSubr _ _) = return s
eval s@(OSyntax _ _) = return s
eval c@(OClosure _ _ _ _) = return c

mapCons :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapCons _ ONil = return ONil
mapCons f (OCons a d) = OCons <$> f a <*> mapCons f d
mapCons _ o = throwError $
	"*** ERROR: proper list required for function application or macro use: "
	++ showObj o

lastCons :: Object -> SchemeM Object
lastCons (OCons a ONil) = return a
lastCons (OCons _ d) = lastCons d
lastCons o = throwError $ "*** ERROR: lastCons: not list: " ++ showObj o

foldlCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldlCons _ o0 ONil = return o0
foldlCons f o0 (OCons a d) = flip (foldlCons f) d =<< f o0 a
foldlCons _ _ _ = throwError "foldlCons: bad"

zipWithCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
zipWithCons f (OCons a d) (OCons a' d') = OCons <$> f a a' <*> zipWithCons f d d'
zipWithCons _ ONil _ = return ONil
zipWithCons _ _ ONil = return ONil
zipWithCons _ _ _ = throwError "zipWithCons: bad"

def :: Object -> Object -> SchemeM Object
def v@(OVar var) val = define var val >> return v
def _ _ = throwError "def: bad"

apply :: EID -> Object -> Object -> Object -> SchemeM Object
apply eid ps as bd = do
	newEnv eid
	_ <- zipWithCons def ps as
	r <- lastCons =<< mapCons eval bd
	popEnv
	return r
