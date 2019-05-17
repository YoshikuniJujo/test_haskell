import Data.List
import Data.Function

haskell :: String
haskell = head . filter ((== '!') . last) $ dec "yg! HHknlsleiapap"

step :: Ord a => [a] -> [[a]] -> [[a]]
step xs xss = zipWith (:) xs $ sortBy (compare `on` head) xss

dec :: Ord a => [a] -> [[a]]
dec xs = iterate (step xs) (map (: []) xs) !! (length xs - 1)
