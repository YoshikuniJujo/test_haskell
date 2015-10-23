import Data.Char

main :: IO ()
main = interact $ unlines . map (map toLower . show
		. (\[s1, s2] -> isCircular s1 s2) . words
	) . tail . lines

isCircular :: String -> String -> Bool
isCircular s1 s2 = s1 `elem` rots s2

rots :: [a] -> [[a]]
rots xs = rotates (length xs) xs

rotates :: Int -> [a] -> [[a]]
rotates 0 _ = []
rotates n xa@(x : xs) = xa : rotates (n - 1) (xs ++ [x])
