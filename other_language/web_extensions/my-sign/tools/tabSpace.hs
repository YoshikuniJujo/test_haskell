{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

main :: IO ()
main = (checkFile `mapM_`) =<< getArgs

checkFile :: FilePath -> IO ()
checkFile fp = do
	putStr $ fp ++ ": "
	putStrLn . check 1 1 =<< readFile fp

check :: Int -> Int -> String -> String
check ln col (' ' : '\t' : _) = "Bad: " ++ show ln ++ ":" ++ show col
check ln col ('\t' : ' ' : _) = "Bad: " ++ show ln ++ ":" ++ show col
check _ _ "" = "OK"
check ln _ ('\n' : cs) = check (ln + 1) 1 cs
check ln col (_ : cs) = check ln (col + 1) cs
