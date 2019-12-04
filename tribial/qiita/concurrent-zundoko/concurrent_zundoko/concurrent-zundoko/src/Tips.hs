{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tips (forkForever, forkLoopIf) where

import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Data.Bool (bool)

forkForever :: IO () -> IO ()
forkForever = void . forkIO . forever

forkLoopIf :: IO Bool -> IO ()
forkLoopIf = void . forkIO . loopIf

loopIf :: Monad m => m Bool -> m ()
loopIf act = bool (return ()) (loopIf act) =<< act
