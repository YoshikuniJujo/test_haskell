{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
	void . forkIO . forever $ threadDelay 200000 >> putStrLn "foo"
	void . forkIO . forever $ threadDelay 300000 >> putStrLn "bar"
	threadDelay 5000000
