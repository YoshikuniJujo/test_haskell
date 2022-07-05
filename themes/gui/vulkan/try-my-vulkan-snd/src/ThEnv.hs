{-# LANGUAGE TemplateHaskell #-}

module ThEnv where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Environment

lookupCompileEnv, lookupCompileEnvExp :: String -> Q Exp
lookupCompileEnv = (`sigE` [t| Maybe String |]) . lift <=< lookupCompileEnvStr
lookupCompileEnvExp = lookupCompileEnv

{-# DEPRECATED lookupCompileEnvExp "use lookupCompileEnv instead" #-}

lookupCompileEnvStr :: String -> Q (Maybe String)
lookupCompileEnvStr key = runIO $ lookup key <$> getEnvironment
