module TestBools (getTestBools, timesDo) where

import Pack

getTestBools :: IO [Bool]
getTestBools = do
	cnt <- readFile "10kFile.txt"
	return $ unpack cnt

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io
