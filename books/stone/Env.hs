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
	popEnv,
	newCEnv,
	intoClosureEnv,
	exitClosureEnv
) where

import Data.Maybe
import "monads-tf" Control.Monad.State

data Object m f
	= ONumber Int
	| OString String
	| OBool Bool
	| ONULL
	| OFunction f
	| OClosure EnvID f
	| ONative ([Object m f] -> m (Object m f))

data EnvID = EnvID Int deriving (Eq, Show)

instance Enum EnvID where
	toEnum = EnvID
	fromEnum (EnvID i) = i

printObject :: Object m a -> IO ()
printObject = putStrLn . showObject

showObject :: Object m a -> String
showObject (ONumber n) = show n
showObject ONULL = "()"
showObject (OFunction _) = "()"
showObject (OClosure _ _) = "()"
showObject o = error $ "showObject: error"

type EnvGen m a = [(String, Object m a)]

type Env m a = ([EnvGen m a], [(EnvID, EnvGen m a)], EnvGen m a)

initialEnv :: Env m a
initialEnv = ([], [], [])

putValueG :: Monad m => String -> Object m a -> StateT (Env m a) m ()
putValueG var val = StateT $ return . (() ,) . putValueG_ var val

putValueG_ :: String -> Object m a -> Env m a -> Env m a
putValueG_ var val ([], cenv, topEnv) = ([], cenv, (var, val) : topEnv)
putValueG_ var val (enva@(env : envs), cenv, topEnv) = case lookup var env of
	Just _ -> (((var, val) : env) : envs, cenv, topEnv)
	_ -> case lookup var topEnv of
		Just _ -> (enva, cenv, (var, val) : topEnv)
		_ -> (((var, val) : env) : envs, cenv, topEnv)

putValue :: Monad m => String -> Object m a -> StateT (Env m a) m ()
putValue var val = StateT $ return . (() ,) . putValue_ var val

putValue_ :: String -> Object m a -> Env m a -> Env m a
putValue_ var val ([], cenv, topEnv) = ([], cenv, (var, val) : topEnv)
putValue_ var val (env : envs, cenv, topEnv) =
	(((var, val) : env) : envs, cenv, topEnv)

getValue :: Monad m => String -> StateT (Env m a) m (Object m a)
getValue var = StateT $ \env -> return (getValue_ var env, env)

getValue_ :: String -> Env m a -> Object m a
getValue_ var ([], _, topEnv) = fromJust $ lookup var topEnv
getValue_ var (env : _, _, topEnv) = case lookup var env of
	Just o -> o
	_ -> fromJust $ lookup var topEnv

newEnv, popEnv :: Monad m => StateT (Env m a) m ()
newEnv = StateT $ return . (() ,) . newEnv_
popEnv = StateT $ return . (() ,) . popEnv_

newEnv_ :: Env m a -> Env m a
newEnv_ (envs, cenv, topEnv) = ([] : envs, cenv, topEnv)

popEnv_ :: Env m a -> Env m a
popEnv_ (_ : envs, cenv, topEnv) = (envs, cenv, topEnv)
popEnv_ _ = error "popEnv: can't pop env"

newCEnv :: Monad m => StateT (Env m a) m EnvID
newCEnv = do
	e <- get
	case e of
		(envs@(env : _), cenvs@((ei, _) : _), topEnv) -> do
			put (envs, (succ ei, env) : cenvs, topEnv)
			return $ succ ei
		(_, cenvs@((ei, _) : _), topEnv) -> do
			put ([], (succ ei, []) : cenvs, topEnv)
			return $ succ ei
		(envs@(env : _), _, topEnv) -> do
			put (envs, [(EnvID 0, env)], topEnv)
			return $ EnvID 0
		(_, _, topEnv) -> do
			put ([], [(EnvID 0, [])], topEnv)
			return $ EnvID 0

intoClosureEnv :: Monad m => EnvID -> StateT (Env m a) m ()
intoClosureEnv eid = do
	(envs, cenvs, topEnv) <- get
	put (fromJust (lookup eid cenvs) : envs, cenvs, topEnv)

exitClosureEnv :: Monad m => EnvID -> StateT (Env m a) m ()
exitClosureEnv eid = do
	(env : envs, cenvs, topEnv) <- get
	put (envs, (eid, env) : cenvs, topEnv)
