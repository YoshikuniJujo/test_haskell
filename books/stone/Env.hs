{-# LANGUAGE PackageImports, TupleSections #-}

module Env (
	Object(..),
	printObject,

	Env,
	initialEnv,
	putValueG,
	putValue,
	getValue,
	newEnv,
	popEnv
) where

import Data.Maybe
import "monads-tf" Control.Monad.State

data Object f
	= ONumber Int
	| OString String
	| OBool Bool
	| ONULL
	| OFunction f
	deriving Show

printObject :: Object a -> IO ()
printObject = putStrLn . showObject

showObject :: Object a -> String
showObject (ONumber n) = show n
showObject ONULL = "()"
showObject (OFunction _) = "()"
showObject o = error $ "showObject: error"

type EnvGen a = [(String, Object a)]

type Env a = ([EnvGen a], EnvGen a)

initialEnv :: Env a
initialEnv = ([], [])

putValueG :: Monad m => String -> Object a -> StateT (Env a) m ()
putValueG var val = StateT $ return . (() ,) . putValueG_ var val

putValueG_ :: String -> Object a -> Env a -> Env a
putValueG_ var val ([], topEnv) = ([], (var, val) : topEnv)
putValueG_ var val (enva@(env : envs), topEnv) = case lookup var env of
	Just _ -> (((var, val) : env) : envs, topEnv)
	_ -> case lookup var topEnv of
		Just _ -> (enva, (var, val) : topEnv)
		_ -> (((var, val) : env) : envs, topEnv)

putValue :: Monad m => String -> Object a -> StateT (Env a) m ()
putValue var val = StateT $ return . (() ,) . putValue_ var val

putValue_ :: String -> Object a -> Env a -> Env a
putValue_ var val ([], topEnv) = ([], (var, val) : topEnv)
putValue_ var val (env : envs, topEnv) = (((var, val) : env) : envs, topEnv)

getValue :: Monad m => String -> StateT (Env a) m (Object a)
getValue var = StateT $ \env -> return (getValue_ var env, env)

getValue_ :: String -> Env a -> Object a
getValue_ var ([], topEnv) = fromJust $ lookup var topEnv
getValue_ var (env : _, topEnv) = case lookup var env of
	Just o -> o
	_ -> fromJust $ lookup var topEnv

newEnv, popEnv :: Monad m => StateT (Env a) m ()
newEnv = StateT $ return . (() ,) . newEnv_
popEnv = StateT $ return . (() ,) . popEnv_

newEnv_ :: Env a -> Env a
newEnv_ (envs, topEnv) = ([] : envs, topEnv)

popEnv_ :: Env a -> Env a
popEnv_ (_ : envs, topEnv) = (envs, topEnv)
popEnv_ _ = error "popEnv: can't pop env"
