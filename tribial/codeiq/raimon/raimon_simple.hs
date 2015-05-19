main :: IO ()
main = interact $ unlines . raimon' . read

raimon' :: Int -> [String]
raimon' = raimon . (+ 1) . (* 4)

raimon :: Int -> [String]
raimon n = zipWith (++)
	(spiral n)
	(map ('#' :) . rotate . rotate $ spiral n)

spiral :: Int -> [String]
spiral 0 = [""]
spiral n = replicate n '#' : map (++ " ") (rotate . spiral $ n - 1)

rotate :: [[a]] -> [[a]]
rotate ss = [ map (!! i) ss | i <- reverse [0 .. length (head ss) - 1] ]
