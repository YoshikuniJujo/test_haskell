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
printAll n ns = putStrLn `mapM_` showAll 5 120 (take n ns)

showAll :: Int -> Int -> [[Integer]] -> [String]
showAll m n = (showLine m n <$>)

showLine :: Int -> Int -> [Integer] -> String
showLine m n = middle n . unwords . (middle m . show <$>)
