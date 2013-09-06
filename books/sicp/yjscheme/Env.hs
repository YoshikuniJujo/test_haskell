{-# LANGUAGE PackageImports #-}

module Env (
	Env, fromList,
	EnvT, runEnvT,
	define,
	getValue,
	throwError, catchError,
) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
-- import Data.Maybe
-- import Control.Applicative

type Env v = [(String, v)]
type EnvT v m = StateT (Env v) (ErrorT String m)

fromList :: [(String, v)] -> Env v
fromList = id

runEnvT :: Monad m => Env v -> EnvT v m a -> m a
runEnvT ie act = do
	er <- runErrorT $ act `evalStateT` ie
	case er of
		Right r -> return r
		_ -> fail "error occur"

define :: (Monad m, Functor m) => String -> v -> EnvT v m ()
define var val = modify ((var, val) :)

getValue :: (Monad m, Functor m) => String -> EnvT v m v 
getValue var = do
	mval <- gets $ lookup var
	case mval of
		Just val -> return val
		_ -> throwError $ "*** ERROR: unbound variable: " ++ var
