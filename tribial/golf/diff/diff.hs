import Control.Arrow

main :: IO ()
main = interact $ (\(t1, t2) -> t1 ++ "\n" ++ t2) . (\(s1 : s2 : _) -> diff s1 s2) . lines

sample1a, sample1b :: String
sample1a = "abcdefghijklmnopqrstuvwxyz"
sample1b = "apple"

diff :: String -> String -> (String, String)
diff "" ys = (map (const '*') ys, ys)
diff xs "" = (xs, map (const '*') xs)
diff xa@(x : xs) ya@(y : ys)
	| x < y = (x :) *** ('*' :) $ diff xs ya
	| x > y = ('*' :) *** (y :) $ diff xa ys
	| otherwise = (x :) *** (y :) $ diff xs ys
