{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.Tips (forkForever) where

import Control.Monad (void, forever)
import Control.Concurrent (forkIO)

forkForever :: IO () -> IO ()
forkForever = void . forkIO . forever
