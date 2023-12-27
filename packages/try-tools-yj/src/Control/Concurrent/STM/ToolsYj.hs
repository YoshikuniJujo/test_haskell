{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.STM.ToolsYj (newDefaultTVar, checkFlag) where

import Control.Concurrent.STM
import Data.Default
import Data.Bool

newDefaultTVar :: Default a => STM (TVar a)
newDefaultTVar = newTVar def

checkFlag :: TVar Bool -> STM Bool
checkFlag flg = readTVar flg >>= bool (pure False) (True <$ writeTVar flg False)
