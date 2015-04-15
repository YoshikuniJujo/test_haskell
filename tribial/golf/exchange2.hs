import Numeric
import Data.List

main :: IO ()
main = interact $ unlines . map (removeLastSpaces . intercalate "  ") . shw . map readRates . lines

maph :: (a -> a) -> [a] -> [a]
maph f (x : xs) = f x : xs

readRates :: String -> (Double, String)
readRates str = let [r, n] = words str in (read r, n)

example :: [(Double, String)]
example = [
	(1, "USD"),
	(0.82, "EUR"),
	(1.23, "AUD"),
	(0.99, "CHF"),
	(1.32, "SGD"),
	(52.03, "RUB") ]

result :: [(Double, String)] -> [[Double]]
result ex = map (\x -> map ((/ x) . fst) ex) (map fst ex)

shw :: [(Double, String)] -> [[String]]
shw ex = ("\\  " : map ((++ " ") . snd) ex) :
	zipWith (:) (map snd ex) (map (map $ showItem) $ result ex)

removeLastSpaces :: String -> String
removeLastSpaces = reverse . dropWhile (==' ') . reverse

showItem :: Double -> String
showItem 1 = "1   "
showItem x = showFFloat (Just 2) x ""
