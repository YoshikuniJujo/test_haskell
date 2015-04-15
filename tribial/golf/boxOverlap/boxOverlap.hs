import Data.List
import System.IO.Unsafe

main :: IO ()
main = interact $ showResult . check . lines

showResult :: String -> String
showResult r@[_] = r
showResult _ = "None"

sample, sample2, sample5, sample6 :: [String]
sample = [
	"a-+",
	"|b|+",
	"+-+|",
	" +-+" ]
sample2 = [
	"1---------------+",
	"|               |",
	"|               |",
	"|        2--------------+",
	"|        |      |       |",
	"+--------|------+       |",
	"         |              |",
	"         +--------------+" ]

sample5 = lines . unsafePerformIO $ readFile "sample5.txt"
sample6 = lines . unsafePerformIO $ readFile "sample6.txt"

topLeft :: [String] -> [(Char, (Int, Int))]
topLeft bs = zip c (zip (cycle iy) ix)
	where
	iy = findIndices (\b -> any (`notElem` " +-|") b) bs
	s = filter (\b -> any (`notElem` " +-|") b) bs
	ix = concatMap (findIndices (`notElem` " +-|") . (bs !!)) iy
	c = concatMap (filter (`notElem` " +-|") . (bs !!)) iy

fourLine :: [String] -> Char -> Int -> Int -> ([String], [String])
fourLine bs c y x = ([top, bottom], [left, right])
	where
	top = takeWhile (/= '+') . drop (x + 1) $ bs !! y
	bottom = takeWhile (/= '+') . drop (x + 1) $ bs !! (y + length left + 1)
	left = takeWhile (/= '+') . drop (y + 1) $ map (!! x) bs
	right = takeWhile (/= '+') . drop (y + 1) $ map (!! (x + length top + 1)) bs

checkLines :: ([String], [String]) -> Bool
checkLines (hrs, vts) = all (all (== '-')) hrs && all (all (== '|')) vts

check :: [String] -> [Char]
check bs = map fst . filter (checkLines . (uncurry $ uncurry . fourLine bs)) $ topLeft bs
