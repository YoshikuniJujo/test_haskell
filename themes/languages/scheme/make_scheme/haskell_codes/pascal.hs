import Control.Applicative
import System.Environment

main :: IO ()
main = getArgs >>=
	putStr . unlines . center
		. map (unwords . map show) . (`take` triangle) . read . head

triangle :: [[Int]]
triangle = iterate (zipWith (+) <$> (++ [0]) <*> (0 :)) [1]

center :: [String] -> [String]
center ss = map (\s -> replicate ((mx - length s) `div` 2) ' ' ++ s) ss
	where mx = maximum $ map length ss
