{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.STM.ToolsYj (newDefaultTVar) where

import Control.Concurrent.STM
import Data.Default

newDefaultTVar :: Default a => STM (TVar a)
newDefaultTVar = newTVar def
