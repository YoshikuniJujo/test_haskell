main :: IO ()
main = interact $ unlines . map (show
		. length . filter (\n -> n `mod` 2 == 0
			|| n `mod` 3 == 0 || n `mod` 5 == 0 || n `mod` 7 == 0)
		. filter (> 0) . numbers . map (read . (: ""))
	) . lines

numbers :: [Int] -> [Int]
numbers (s : xs) = numbers_ s xs

numbers_ :: Int -> [Int] -> [Int]
numbers_ s (x : xs) =
	numbers_ (s * 10 + x) xs ++ map (s +) (numbers_ x xs) ++ map (s -) (numbers_ x xs)
numbers_ s _ = [s]
