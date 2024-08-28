{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.STM.ToolsYj (newDefaultTVar, readModifyTVar, checkFlag) where

import Control.Concurrent.STM
import Data.Default
import Data.Bool

newDefaultTVar :: Default a => STM (TVar a)
newDefaultTVar = newTVar def

readModifyTVar :: TVar a -> (a -> a) -> STM a
readModifyTVar v f = readTVar v <* modifyTVar v f

checkFlag :: TVar Bool -> STM Bool
checkFlag flg = readTVar flg >>= bool (pure False) (True <$ writeTVar flg False)
