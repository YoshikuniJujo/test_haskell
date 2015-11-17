import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . ones . map (read . (: "")) . head . lines

ones :: [Int] -> Int
ones (0 : ds) = ones ds
ones [n] = 1
ones (1 : ds) = let n = length ds in n * 10 ^ (n - 1) + dec ds + 1 + ones ds
ones (d : ds) = let n = length ds in d * n * 10 ^ (n - 1) + 10 ^ n + ones ds
ones _ = 0

dec :: [Int] -> Int
dec = foldl' ((+) . (* 10)) 0
