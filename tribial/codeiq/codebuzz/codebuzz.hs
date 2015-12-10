import Data.List

main :: IO ()
main = interact $ (++ "\n") . intercalate ","
	. map (show . uncurry conv) . zip [0 ..] . split ',' . head . lines

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split s (x : xs)
	| x == s = [] : split s xs
	| otherwise = heading (x :) $ split s xs
	where heading f (y : ys) = f y : ys

conv :: Int -> String -> Int
conv _ "Code" = 3
conv _ "IQ" = 5
conv _ "CodeIQ" = 15
conv i _ = i
