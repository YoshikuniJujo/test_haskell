{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.Tips (forkForever, forkLoopIf) where

import Control.Monad (void, forever)
import Control.Monad.Tips (loopIf)
import Control.Concurrent (forkIO)

forkForever :: IO () -> IO ()
forkForever = void . forkIO . forever

forkLoopIf :: IO Bool -> IO ()
forkLoopIf = void . forkIO . loopIf
