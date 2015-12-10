import Data.List

data Card = Card Bool Int deriving (Show, Eq)

cards :: [Int] -> [Card]
cards = map (Card True)

rev :: Card -> Card
rev (Card b n) = Card (not b) n

upright :: Card -> Bool
upright (Card b _) = b

turn1 :: [Card] -> [Card]
turn1 (c@(Card _ n) : cs) =
	rev `map` (take (n - 1) cs) ++ drop (n - 1) cs ++ [rev c]

check :: [[Card]] -> [[Card]] -> Bool
check pcss (cs : css)
	| all (not . upright) cs = True
	| cs `elem` pcss = False
	| otherwise = check (cs : pcss) css

main :: IO ()
main = print . length
	. filter (check [] . iterate turn1 . cards) $ permutations [1 .. 7]
