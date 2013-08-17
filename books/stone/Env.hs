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

putValueG :: String -> Object a -> Env a -> Env a
putValueG var val ([], topEnv) = ([], (var, val) : topEnv)
putValueG var val (enva@(env : envs), topEnv) = case lookup var env of
	Just _ -> (((var, val) : env) : envs, topEnv)
	_ -> case lookup var topEnv of
		Just _ -> (enva, (var, val) : topEnv)
		_ -> (((var, val) : env) : envs, topEnv)

putValue :: String -> Object a -> Env a -> Env a
putValue var val ([], topEnv) = ([], (var, val) : topEnv)
putValue var val (env : envs, topEnv) = (((var, val) : env) : envs, topEnv)

getValue :: String -> Env a -> Object a
getValue var ([], topEnv) = fromJust $ lookup var topEnv
getValue var (env : _, topEnv) = case lookup var env of
	Just o -> o
	_ -> fromJust $ lookup var topEnv

newEnv :: Env a -> Env a
newEnv (envs, topEnv) = ([] : envs, topEnv)

popEnv :: Env a -> Env a
popEnv (_ : envs, topEnv) = (envs, topEnv)
popEnv _ = error "popEnv: can't pop env"
