{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.IORef

main :: IO ()
main = print =<< mySum [123, 456, 789]

mySum :: [Int] -> IO Int
mySum nums = do
	s <- newIORef 0
	(modifyIORef s . (+)) `mapM_` nums
	readIORef s
