{-# LANGUAGE TemplateHaskell #-}

module ThEnv where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Environment

lookupCompileEnvExp :: String -> Q Exp
lookupCompileEnvExp = (`sigE` [t| Maybe String |]) . lift <=< lookupCompileEnv

lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = runIO $ lookup key <$> getEnvironment
