w, h :: Int
w = 79
h = 39
sz = 30

incircle :: Int -> Int -> Int -> Int -> Int -> Bool
incircle w h r x y = (x - w `div` 2) ^ 2 + ((y - h `div` 2) * 2) ^ 2 < r ^ 2

main :: IO ()
main = putStr $ unlines [
	[ if incircle w h sz x y then '*' else '-' | x <- [0 .. w] ] | y <- [0 .. h]]
