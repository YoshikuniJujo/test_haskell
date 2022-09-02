import System.Environment

main = putStrLn . concat . (`replicate` "hello") . read . head =<< getArgs
