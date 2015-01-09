{-# LANGUAGE PackageImports, TupleSections #-}

module Env (
	Object(..),
	printObject,

	Env,
	mkInitialEnv,
	putValueG,
	putValue,
	getValue,
	newEnv,
	popEnv,
	newCEnv,
	intoClosureEnv,
	exitClosureEnv,

	newArray,
	getArray,
	putToArray
) where

import Data.Maybe
import "monads-tf" Control.Monad.State
import Data.Array
import Data.List

data Object m b
	= ONumber Integer
	| OString String
	| OBool Bool
	| ONULL
	| OFunction ([String], b)
	| OClosure EnvID ([String], b)
	| ONative ([Object m b] -> m (Object m b))
	| OClass (Maybe String) b
	| OObject EnvID
	| OArray ArrID

data EnvID = EnvID Int deriving (Eq, Show)
data ArrID = ArrID Int deriving (Eq, Show)

instance Enum EnvID where
	toEnum = EnvID
	fromEnum (EnvID i) = i

instance Enum ArrID where
	toEnum = ArrID
	fromEnum (ArrID i) = i

printObject :: Object m a -> IO ()
printObject = putStrLn . showObject

showObject :: Object m a -> String
showObject (OString s) = show s
showObject (ONumber n) = show n
showObject ONULL = "()"
showObject (OFunction _) = "()"
showObject (OClosure _ _) = "()"
showObject (ONative _) = "()"
showObject (OClass _ _) = "()"
showObject (OObject eid) = "(object: " ++ show eid ++ ")"
showObject (OArray aid) = "(array: " ++ show aid ++ ")"
-- showObject (OArray a) = "[" ++ intercalate ", " (map showObject $ elems a) ++ "]"
showObject o = error $ "showObject: error"

type EnvGen m a = [(String, Object m a)]
type ArrEnv m a = [(ArrID, Array Integer (Object m a))]

type Env m a = ([EnvGen m a], [(EnvID, EnvGen m a)], EnvGen m a, ArrEnv m a)

mkInitialEnv :: EnvGen m a -> Env m a
mkInitialEnv ie = ([], [], ie, [])

putValueG :: Monad m => String -> Object m a -> StateT (Env m a) m ()
putValueG var val = StateT $ return . (() ,) . putValueG_ var val

putValueG_ :: String -> Object m a -> Env m a -> Env m a
putValueG_ var val ([], cenv, topEnv, arrEnv) =
	([], cenv, (var, val) : topEnv, arrEnv)
putValueG_ var val (enva@(env : envs), cenv, topEnv, arrEnv) = case lookup var env of
	Just _ -> (((var, val) : env) : envs, cenv, topEnv, arrEnv)
	_ -> case lookup var topEnv of
		Just _ -> (enva, cenv, (var, val) : topEnv, arrEnv)
		_ -> (((var, val) : env) : envs, cenv, topEnv, arrEnv)

putValue :: Monad m => String -> Object m a -> StateT (Env m a) m ()
putValue var val = StateT $ return . (() ,) . putValue_ var val

putValue_ :: String -> Object m a -> Env m a -> Env m a
putValue_ var val ([], cenv, topEnv, arrEnv) =
	([], cenv, (var, val) : topEnv, arrEnv)
putValue_ var val (env : envs, cenv, topEnv, arrEnv) =
	(((var, val) : env) : envs, cenv, topEnv, arrEnv)

getValue :: Monad m => String -> StateT (Env m a) m (Object m a)
getValue var = StateT $ \env -> return (getValue_ var env, env)

getValue_ :: String -> Env m a -> Object m a
getValue_ var ([], _, topEnv, arrEnv) = fromJust $ lookup var topEnv
getValue_ var (env : _, _, topEnv, arrEnv) = case lookup var env of
	Just o -> o
	_ -> fromJust $ lookup var topEnv

newEnv, popEnv :: Monad m => StateT (Env m a) m ()
newEnv = StateT $ return . (() ,) . newEnv_
popEnv = StateT $ return . (() ,) . popEnv_

newEnv_ :: Env m a -> Env m a
newEnv_ (envs, cenv, topEnv, arrEnv) = ([] : envs, cenv, topEnv, arrEnv)

popEnv_ :: Env m a -> Env m a
popEnv_ (_ : envs, cenv, topEnv, arrEnv) = (envs, cenv, topEnv, arrEnv)
popEnv_ _ = error "popEnv: can't pop env"

newCEnv :: Monad m => StateT (Env m a) m EnvID
newCEnv = do
	e <- get
	case e of
		(envs@(env : _), cenvs@((ei, _) : _), topEnv, arrEnv) -> do
			put (envs, (succ ei, env) : cenvs, topEnv, arrEnv)
			return $ succ ei
		(_, cenvs@((ei, _) : _), topEnv, arrEnv) -> do
			put ([], (succ ei, []) : cenvs, topEnv, arrEnv)
			return $ succ ei
		(envs@(env : _), _, topEnv, arrEnv) -> do
			put (envs, [(EnvID 0, env)], topEnv, arrEnv)
			return $ EnvID 0
		(_, _, topEnv, arrEnv) -> do
			put ([], [(EnvID 0, [])], topEnv, arrEnv)
			return $ EnvID 0

intoClosureEnv :: Monad m => EnvID -> StateT (Env m a) m ()
intoClosureEnv eid = do
	(envs, cenvs, topEnv, arrEnv) <- get
	put (fromJust (lookup eid cenvs) : envs, cenvs, topEnv, arrEnv)

exitClosureEnv :: Monad m => EnvID -> StateT (Env m a) m ()
exitClosureEnv eid = do
	(env : envs, cenvs, topEnv, arrEnv) <- get
	put (envs, (eid, env) : cenvs, topEnv, arrEnv)

newArray :: Monad m => Array Integer (Object m a) -> StateT (Env m a) m ArrID
newArray arr = do
	(es, ces, te, ae) <- get
	case ae of
		(aid, _) : _ -> do
			put (es, ces, te, (succ aid, arr) : ae)
			return $ succ aid
		_ -> do	put (es, ces, te, [(ArrID 0, arr)])
			return $ ArrID 0

getArray :: Monad m => ArrID -> StateT (Env m a) m (Array Integer (Object m a))
getArray aid = do
	(_, _, _, ae) <- get
	return $ fromJust $ lookup aid ae

putToArray :: Monad m => ArrID -> Integer -> Object m a -> StateT (Env m a) m ()
putToArray aid i x = do
	(es, ces, te, ae) <- get
	let arr = fromJust $ lookup aid ae
	put (es, ces, te, (aid, arr // [(i, x)]) : ae)
