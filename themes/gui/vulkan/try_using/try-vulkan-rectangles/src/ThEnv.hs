{-# LANGUAGE TemplateHaskell #-}

module ThEnv where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Environment

lookupCompileEnv :: String -> Q Exp
lookupCompileEnv = (`sigE` [t| Maybe String |]) . lift <=< lookupCompileEnvStr

lookupCompileEnvStr :: String -> Q (Maybe String)
lookupCompileEnvStr key = runIO $ lookup key <$> getEnvironment
