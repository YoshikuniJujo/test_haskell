{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (abs)
import ContinuationPassingStyle

main :: IO ()
main = do
	putStrLn "List Concatenation with CPS Style"
	putChar . last . abs . foldl1 (.++.) $ rep <$> replicate 10000 "hello"
	putStrLn ""
