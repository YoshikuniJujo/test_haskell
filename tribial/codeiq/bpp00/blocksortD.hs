import Data.List
import Data.Function

main :: IO ()
main = interact $ take 1000 . unblock . chop

unblock :: Ord a => [a] -> [a]
unblock xs = map (xs !!) . toPos 0 $ sortRule xs

sortRule :: Ord a => [a] -> [Int]
sortRule = map fst . sortBy (compare `on` snd) . zip [0 ..]

toPos :: Int -> [Int] -> [Int]
toPos n is = n : toPos (is !! n) is

chop :: String -> String
chop "\n" = ""
chop (c : cs) = c : chop cs
