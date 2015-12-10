import Data.List
import Data.Function
import System.Environment

import System.IO.Unsafe

sampleRaw :: String
sampleRaw = unsafePerformIO $ readFile "towers.txt"

makePreTowers :: (Int, [Int], [Int]) -> (Int, [(Int, Int)])
makePreTowers (t, xs, hs) = (t, zip xs hs)

readTowersListFromRaw :: [String] -> [[((Int, Int), Int)]]
readTowersListFromRaw (n : ds) =
	map (uncurry makeTowers . makePreTowers) $ readTowersList (read n) ds

readTowersList :: Int -> [String] -> [(Int, [Int], [Int])]
readTowersList 0 src = []
readTowersList n src =
	let (ts, r) = readTowers src in ts : readTowersList (n - 1) r

readTowers :: [String] -> ((Int, [Int], [Int]), [String])
readTowers (n_ : t : ts) = ((read t, map read xs, map read hs), r')
	where
	n = read n_
	(xs, r) = splitAt n ts
	(hs, r') = splitAt n r
readTowers _ = error "parse error"

makeTower :: Int -> (Int, Int) -> ((Int, Int), Int)
makeTower t (x, h) = ((x - t, x + t), h)

makeTowers :: Int -> [(Int, Int)] -> [((Int, Int), Int)]
makeTowers t = map (makeTower t) . sortBy (on compare fst)

towers :: [((Int, Int), Int)]
towers = [
	((0, 4), 6),
	((1, 5), 8),
	((3, 7), 9),
	((4, 8), 3),
	((5, 9), 7) ]

calc :: ((Int, Int), Int) -> ((Int, Int), Int) -> Int
calc ((xmn, xmx), n) ((xmn', xmx'), _)
	| xmn <= xmn' && xmn' <= xmx && xmx <= xmx' = n `div'` d
	| xmx < xmn' = n `div'` d'
	where
	d = xmn' - xmn
	d' = xmx - xmn + 1
calc _ _ = error "bad data"

div' :: Int -> Int -> Int
div' m n = m `div` n + if m `mod` n == 0 then 0 else 1

append :: ((Int, Int), Int) -> ((Int, Int), Int) -> ((Int, Int), Int)
append ((xmn, _), n1) ((_, xmx), n2) = ((xmn, xmx), n1 + n2)

processHead :: [((Int, Int), Int)] -> Maybe [((Int, Int), Int)]
processHead [] = Nothing
processHead [t] = Nothing
processHead [t1, t2@((xmn, xmx), n)]
	| calc t1 t2 >= n `div'` (xmx - xmn + 1) = Just [append t1 t2]
	| otherwise = Nothing
processHead (t1 : t2 : ts@(t3 : _))
	| calc t1 t2 >= calc t2 t3 = Just $ append t1 t2 : ts
	| otherwise = Nothing

process1 :: [((Int, Int), Int)] -> [((Int, Int), Int)]
process1 [] = []
process1 [t] = [t]
process1 ta@(t : ts) = case processHead ta of
	Just ta' -> process1 ta'
	_ -> t : process1 ts

fixed :: Eq a => (a -> a) -> a -> a
fixed f x
	| x == f x = x
	| otherwise = fixed f (f x)

marged :: [((Int, Int), Int)] -> [((Int, Int), Int)]
marged = fixed process1

higest :: [((Int, Int), Int)] -> Int
higest ts = n `div'` (xmx - xmn + 1)
	where ((xmn, xmx), n) = last $ fixed process1 ts

t1, t2, t3 :: [((Int, Int), Int)]
[t1, t2, t3] = readTowersListFromRaw $ words sampleRaw

main :: IO ()
main = do
	fp : _ <- getArgs
	src <- readFile fp
	print . map marged . readTowersListFromRaw $ words src
	print . map higest . readTowersListFromRaw $ words src
