{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.IO.Unsafe

check :: Int -> IO [Int]
check n = unsafeInterleaveIO $ do
	putStrLn $ "<n = " ++ show n ++ ">"
	(n :) <$> check (n + 1)
