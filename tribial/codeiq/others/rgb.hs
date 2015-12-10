data RGB = R | G | B deriving (Show, Eq)

draw :: Int -> [[RGB]]
draw 0 = [[]]
draw n = do
	rgbs <- draw $ n - 1
	map (: rgbs) [ x | x <- [R, G, B], null rgbs || x /= head rgbs ]

main :: IO ()
main = putStr . unlines $
	let rgbs = draw 5 in map (concatMap show) rgbs ++ [show $ length rgbs]
