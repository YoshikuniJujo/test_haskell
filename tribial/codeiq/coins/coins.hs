import Data.Maybe
import Data.List
import Data.Function
import Data.Char

main :: IO ()
main = interact $ (++ "\n") . show
	. coins . makePaths . sortBy (on compare snd)
	. readInput

type Pos = (Int, Int)
type Time = Int

sample1, sample2, sample3 :: [(Pos, Time)]
sample1 = [((1, 1), 2), ((2, 2), 4), ((3, 3), 6), ((4, 4), 8), ((5, 5), 10)]
sample2 = [((1, 1), 2), ((1, -1), 2), ((-1, 1), 2), ((-1, -1), 2), ((2, 2), 4)]
sample3 = [((1, 1), 3), ((1, -1), 2), ((-1, 1), 2), ((-1, -1), 2), ((2, 2), 4)]

data Point
	= Start
	| Point Int
	deriving (Show, Eq)

instance Enum Point where
	toEnum 0 = Start
	toEnum n = Point n
	fromEnum Start = 0
	fromEnum (Point n) = n

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

accessible :: (Pos, Time) -> (Pos, Time) -> Bool
accessible (p1, t1) (p2, t2) = distance p1 p2 <= t2 - t1

paths :: [(Point, (Pos, Time))] -> [(Point, [Point])]
paths [] = []
paths ((pnt0, pt0) : ps) =
	(pnt0, map fst $ filter (accessible pt0 . snd) ps) : paths ps

makePaths :: [(Pos, Time)] -> [(Point, [Point])]
makePaths = paths . (zip [Start ..]) . (((0, 0), 0) :)

coins :: [(Point, [Point])] -> Int
coins pths = fromJust $ lookup Start css
	where
	pths' = reverse pths
	css = get pths'
	get [] = []
	get ((p, []) : pss) = (p, 0) : get pss
	get ((p, ps) : pss) =
		(p, 1 + maximum (map (fromJust . flip lookup css) ps)) : get pss

readInput :: String -> [(Pos, Time)]
readInput (c : s) | isSpace c = readInput s
readInput ('{' : s) = readCoins $ dropWhile isSpace s

readCoins :: String -> [(Pos, Time)]
readCoins (c : s) | isSpace c = readCoins s
readCoins (',' : s) = readCoins s
readCoins ('}' : s) = []
readCoins s = let (c, r) = readCoin $ dropWhile isSpace s in c : readCoins r

readCoin :: String -> ((Pos, Time), String)
readCoin ('{' : s) = case span (/= ',') s of
	(x, _ : r) -> case span (/= ',') r of
		(y, _ : r') -> case span (/= '}') r' of
			(t, _ : r'') -> (((read x, read y), read t), r'')
