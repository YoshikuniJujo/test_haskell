{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import PerformanceProblems

main :: IO ()
main = do
	putStrLn "List Concatenation"
	putChar . last $ foldl1 (.++) $ replicate 10000 "hello"
	putStrLn ""
