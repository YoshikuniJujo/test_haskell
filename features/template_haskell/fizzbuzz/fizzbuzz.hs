{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Control.Monad.IO.Class

do	
	liftIO $ forM_ [1 :: Int .. 100] $ \n -> do
		putStrLn $ case (n `mod` 3, n `mod` 5) of
			(0, 0) -> "FizzBuzz"
			(0, _) -> "Fizz"
			(_, 0) -> "Buzz"
			_ -> show n
	return []

main :: IO ()
main = return ()
