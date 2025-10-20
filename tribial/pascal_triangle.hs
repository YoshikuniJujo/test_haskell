module Main where

main :: IO ()
main = putStrLn "foobar"

pascalTriangle :: [[Integer]]
pascalTriangle = iterate next [1]

next :: [Integer] -> [Integer]
next a@(_ : t) = 1 : zipWith (+) a t ++ [1]

middle :: Int -> String -> String
middle n s = replicate ((n - length s) `div` 2) ' ' ++ s

printAll :: Int -> [[Integer]] -> IO ()
printAll n ns = putStrLn `mapM_` showAll 80 (take n ns)

showAll :: Int -> [[Integer]] -> [String]
showAll n = (showLine n <$>)

showLine :: Int -> [Integer] -> String
showLine n = middle n . unwords . (show <$>)
