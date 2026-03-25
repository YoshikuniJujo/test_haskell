module Main (main) where

main :: IO ()
main = do
	putStrLn "*** BEGIN HELLO ***"
	putStrLn "world.txt"
	putStrLn =<< readFile "world.txt"
	putStrLn "hello.txt"
	putStrLn =<< readFile "hello.txt"
	putStrLn "*** END HELLO ***"
