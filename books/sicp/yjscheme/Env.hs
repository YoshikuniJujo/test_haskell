{-# LANGUAGE PackageImports #-}

module Env (
	Environment, mkInitEnv,
	EnvT, runEnvT,
	define,
	getValue,
	getEID, EID,
	newEnv, popEnv,
	throwError, catchError,
) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Map

type EnvT v m = StateT (Environment v) (ErrorT String m)

type Environment v = (EID, ([EID], Map EID (Env1 v)))
data Env1 v = Env {
	outEnvID :: Maybe EID,
	envBody :: Env v
 }
type EID = Int 
type Env v = [(String, v)]

mkInitEnv :: [(String, v)] -> Environment v
mkInitEnv e = (0, ([0], fromList [(0, Env Nothing e)]))

getEnv :: EID -> Map EID (Env1 v) -> Env1 v
getEnv eid envs = case Data.Map.lookup eid envs of
	Just e -> e
	_ -> error "bad"

getV :: String -> Environment v -> Maybe v
getV var (_, (eid : _, envs)) = let env = getEnv eid envs in
	case (Prelude.lookup var $ envBody env, outEnvID env) of
		(val@(Just _), _) -> val
		(_, Just oeid) -> getV var (undefined, ([oeid], envs))
		_ -> Nothing
getV _ _ = error "bad"

def :: String -> v -> Environment v -> Environment v
def var val (meid, (eids@(eid : _), envs)) = let
	Env oeid body = getEnv eid envs
	env = Env oeid $ (var, val) : body in
	(meid, (eids, insert eid env envs))
def  _ _ _ = error "bad"

runEnvT :: Monad m => Environment v -> EnvT v m a -> m a
runEnvT ie act = do
	er <- runErrorT $ act `evalStateT` ie
	case er of
		Right r -> return r
		_ -> fail "error occur"

define :: (Monad m, Functor m) => String -> v -> EnvT v m ()
define var val = modify $ def var val

getValue :: (Monad m, Functor m) => String -> EnvT v m v 
getValue var = do
	mval <- gets $ getV var
	case mval of
		Just val -> return val
		_ -> throwError $ "*** ERROR: unbound variable: " ++ var

getEID :: Monad m => EnvT v m EID
getEID = gets $ head . fst . snd

newEnv :: Monad m => EID -> EnvT v m ()
newEnv outer = do
	(meid, (eids, envs)) <- get
	let	meid' = succ meid
		env = Env (Just outer) []
	put (meid', (meid' : eids, insert meid' env envs))

popEnv :: Monad m => EnvT v m ()
popEnv = do
	(meid, (_ : eids, envs)) <- get
	put (meid, (eids, envs))
