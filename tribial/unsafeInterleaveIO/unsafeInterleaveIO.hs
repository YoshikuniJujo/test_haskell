{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Control.DeepSeq
import Control.Exception
import System.IO.Unsafe

check :: Int -> IO [Int]
check n = unsafeInterleaveIO $ do
	putStrLn $ "<n = " ++ show n ++ ">"
	(n :) <$> check (n + 1)

toTen :: Int -> IO [Int]
toTen n = unsafeInterleaveIO $ do
	when (n > 10) $ error "Expected: n <= 10"
	putStrLn $ "<n = " ++ show n ++ ">"
	(n :) <$> toTen (n + 1)

escape :: IO ()
escape = do
	ns <- toTen 0 `catch` \(_e :: ErrorCall) -> putStrLn "catched" >> return []
	print $ take 20 ns

notEscape :: IO ()
notEscape = do
	ns <- (evaluate . force =<< toTen 0) `catch`
		\(_e :: ErrorCall) -> putStrLn "catched" >> return []
	print $ take 20 ns
