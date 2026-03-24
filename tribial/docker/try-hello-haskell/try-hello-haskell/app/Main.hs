module Main (main) where

main :: IO ()
main = putStrLn =<< readFile "hello.txt"
