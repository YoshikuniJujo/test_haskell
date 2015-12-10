import Data.List

main :: IO ()
main = interact $ show . mtf []

mtf :: Eq a => [a] -> [a] -> [Either Int a]
mtf _ [] = []
mtf t (x : xs) = case elemIndex x t of
	Just i -> Left i : mtf (x : delete x t) xs
	_ -> Right x : mtf (x : t) xs
