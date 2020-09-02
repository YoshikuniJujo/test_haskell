{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent

main :: IO ()
main = nestN 100

nestN :: Int -> IO ()
nestN n | n < 1 = pure ()
nestN n = (print =<< myThreadId) >> void (forkIO . nestN $ n - 1)
