import Control.Arrow

main :: IO ()
main = interact $ (++ "\n")
	. show . (\((w, h), n, as) -> machiawase n w h as) . parse . lines

parse :: [String] -> ((Int, Int), Int, [Int])
parse (wh : n : as : _) = ((w, h), read n, map read $ words as)
	where [w, h] = map read $ words wh

machiawase :: Int -> Int -> Int -> [Int] -> Int
machiawase n w h hs = dists n (1, w) (1, h) (map (position w h) hs)

position :: Int -> Int -> Int -> (Int, Int)
position w h = (ps !!) . (subtract 1)
	where ps = [ (x, y) | y <- [1 .. h], x <- [1 .. w] ]

dists :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int
dists n (mnx, mxx) (mny, mxy) = (\(dx, dy) -> dx + dy) .
	(diffs n mnx mxx *** diffs n mny mxy) . unzip

diffs :: Int -> Int -> Int -> [Int] -> Int
diffs n mn mx xs = sum $ map (abs . subtract (median n mn mx xs)) xs

median :: Int -> Int -> Int -> [Int] -> Int
median n mn mx xs = sellect (n `div` 2, n) (mn, mx) xs

sellect :: (Int, Int) -> (Int, Int) -> [Int] -> Int
sellect (i, n) (mn, mx) xs
	| i <= 0 = minimum xs
	| n - 1 <= i = maximum xs
	| i < ls = sellect (i, ls) (mn, pvt) sms
	| i < n - lb = pvt
	| otherwise = sellect (i - (n - lb), lb) (pvt, mx) bgs
		where
		pvt = let p = mn + (mx - mn) * i `div` n in
			if p == mn then p + 1 else p
		sms = filter (< pvt) xs
		bgs = filter (pvt <) xs
		ls = length sms
		lb = length bgs
