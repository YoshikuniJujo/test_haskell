main :: IO ()
main = print $ steps !! 30

steps :: [Int]
steps@(_ : tSteps@(_ : ttSteps)) =
	1 : 1 : 2 : zipWith3 (\x y z -> x + y + z) steps tSteps ttSteps
