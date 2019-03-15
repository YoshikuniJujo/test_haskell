{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module SetLang (Lang) where

import Control.Monad.IO.Class
import System.Environment
import Language.Haskell.TH

type Lang = $(litT . strTyLit =<< liftIO (take 2 <$> getEnv "LANG"))
