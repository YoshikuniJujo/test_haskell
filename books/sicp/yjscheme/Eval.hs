{-# LANGUAGE PackageImports #-}

module Eval (
	eval,

	foldListl,
	zipWithList,
	lastList,
	mapList,
) where

import Env
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Applicative

eval :: Object -> SchemeM Object
eval i@(OInt _) = return i
eval d@(ODouble _) = return d
eval s@(OString _) = return s
eval (OVar var) = do
	((_, h), _) <- get
	let lval = case h of
		Just (_, henv) -> lookup (EVar var) henv
		Nothing -> Nothing
	mval <- gets $ (lookup $ EVar var) . snd
	case (lval, mval) of
		(Just v, _) -> return v
		(_, Just val) -> return val
		(_, _) -> throwError $ "*** ERROR: unbound variable: " ++ var
			++ " " ++ show h
eval o@(OCons f_ args_) = do
	f <- eval f_
	case f of
		OSubr _ s -> s =<< mapList eval args_
		OSyntax _ s -> s args_
		OClosure _ eid as bd -> do
			args <- mapList eval args_
			eid' <- nowEnv
			intoEnv eid
			r <- apply as args bd
			intoEnv eid'
			return $ lastList r
		_ -> throwError $ "*** ERROR: invalid application: " ++ showObj o
eval ONil = return ONil
eval o@(OBool _) = return o
eval o@(OSyntax _ _) = return o
eval _ = throwError "eval: not yet constructed"

apply :: Object -> Object -> Object -> SchemeM Object
apply vs as bd = do
	_ <- zipWithList def vs as
	mapList eval bd

def :: Object -> Object -> SchemeM Object
def v@(OVar var) val = do
	((maxID, Just (hid, henv)), tenv) <- get
	put ((maxID, Just (hid, (EVar var, val) : henv)), tenv)
	return v
def _ _ = throwError "def: bad"

zipWithList :: (Object -> Object -> SchemeM Object) ->
	Object -> Object -> SchemeM Object
zipWithList f (OCons a d) (OCons a' d') =
	OCons <$> (f a a') <*> zipWithList f d d'
zipWithList _ ONil _ = return ONil
zipWithList _ _ ONil = return ONil
zipWithList _ _ _ = throwError "zipWithList: bad"

lastList :: Object -> Object
lastList (OCons a ONil) = a
lastList (OCons _ d) = lastList d
lastList _ = error "lastList: bad"

mapList :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapList _ ONil = return ONil
mapList f (OCons a d) = OCons <$> f a <*> mapList f d
mapList _ _ = throwError "mapList: not list"

foldListl :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldListl _ o0 ONil = return o0
foldListl f o0 (OCons a d) = flip (foldListl f) d =<< f o0 a
foldListl _ _ _ = throwError "foldListl: not list"
