module Eval (
	eval,
	Env, nullEnv,
	EnvT, runEnvT,
	Object(..), showObj,
	throwError, catchError,
	foldlCons,
	SchemeM
) where

import Object

import Control.Applicative

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval (OVar v) = getValue v
eval (OCons f_ as_) = do
	f <- eval f_
	case f of
		OSubr _ s -> s =<< mapCons eval as_
		_ -> error "eval: bad"
eval ONil = return ONil
eval s@(OSubr _ _) = return s
eval u@OUndef = return u

mapCons :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapCons _ ONil = return ONil
mapCons f (OCons a d) = OCons <$> f a <*> mapCons f d
mapCons _ o = throwError $
	"*** ERROR: proper list required for function application or macro use: "
	++ showObj o

foldlCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldlCons _ o0 ONil = return o0
foldlCons f o0 (OCons a d) = flip (foldlCons f) d =<< f o0 a
foldlCons _ _ _ = throwError "foldlCons: bad"
