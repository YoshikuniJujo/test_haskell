data Rotate = L | R deriving Show

raimon' :: Int -> [String]
raimon' = raimon . (+ 1) . (* 4)

raimon :: Int -> [String]
raimon n = zipWith (++)
	(spiral L n)
	(map ('#' :) . rotate L . rotate L $ spiral L n)

spiralL :: Int -> [String]
spiralL 0 = [""]
spiralL n = replicate n '#' : map (++ " ") (rotateL . spiralL $ n - 1)

spiral :: Rotate -> Int -> [String]
spiral _ 0 = [""]
spiral r n = replicate n '#' : map pad (rotate r . spiral r $ n - 1)
	where
	pad = case r of L -> (++ " "); _ -> (' ' :)

sample :: [String]
sample = [
	"12345",
	"67890",
	"abcde",
	"fghij",
	"klmno"
	]

rotateL :: [[a]] -> [[a]]
rotateL ss = [ map (!! i) ss | i <- reverse [0 .. length (head ss) - 1] ]

rotate :: Rotate -> [[a]] -> [[a]]
rotate r ss = [ map (!! i) $ rvR ss | i <- rvL [0 .. length (head ss) - 1] ]
	where
	rvL = case r of L -> reverse; _ -> id
	rvR = case r of R -> reverse; _ -> id
