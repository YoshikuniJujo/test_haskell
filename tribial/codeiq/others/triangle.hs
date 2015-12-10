main :: IO ()
main = interact $ (++ "\n") . show . length . triangles . read . head . lines

triangles :: Int -> [(Int, Int, Int)]
triangles n = [ (a, b, c) |
	a <- [1 .. n], b <- [a .. n], c <- [b .. min n $ a + b - 1] ]
