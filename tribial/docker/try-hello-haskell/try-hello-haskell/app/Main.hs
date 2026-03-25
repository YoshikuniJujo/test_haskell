module Main (main) where

main :: IO ()
main = do
	putStrLn "*** BEGIN HELLO ***"
	putStrLn =<< readFile "world.txt"
	putStrLn =<< readFile "hello.txt"
	putStrLn "*** END HELLO ***"
