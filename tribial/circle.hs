calcX :: Int -> Int -> Int
calcX r y = last $ takeWhile ((<= r ^ 2) . (+ y ^ 2) . (^ 2)) [0 ..]

circle :: Int -> String
circle r = unlines $ map (mkLine r . calcX r) [- r .. r]

mkLine :: Int -> Int -> String
mkLine r x = replicate (r - x) ' ' ++ replicate (x * 2) '*'

mkLine' :: Int -> Int -> String
mkLine' r x = replicate (r * 2 - x * 2) ' ' ++ replicate (x * 4) '*'

calcX2 :: Int -> Int -> Int
calcX2 r_ y_ = last $ takeWhile ((<= r ^ 2) . (+ y ^ 2) . (^ 2)) [0 ..]
	where r = r_ * 2; y = y_ * 2

circle2 :: Int -> String
circle2 r = unlines $ map (mkLine2 r . calcX2 r) [- r .. r]

mkLine2 :: Int -> Int -> String
mkLine2 r x = replicate (r * 2 - x) ' ' ++ replicate (x * 2) '*'
