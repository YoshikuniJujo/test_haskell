import Control.Arrow

main :: IO ()
main = interact $ (++ "\n") . showRL . runLength . chop

showRL :: [(Char, Int)] -> String
showRL = concatMap (\(c, n) -> c : show n)

runLength :: Eq a => [a] -> [(a, Int)]
runLength [x] = [(x, 1)]
runLength (x : xs@(y : _))
	| x == y = heading (second (+ 1)) $ runLength xs
	| otherwise = (x, 1) : runLength xs
runLength _ = []

heading :: (a -> a) -> [a] -> [a]
heading f (x : xs) = f x : xs

chop :: String -> String
chop "\n" = ""
chop (c : cs) = c : chop cs
