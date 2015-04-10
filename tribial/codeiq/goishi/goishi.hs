-- 29個で4個連続
-- 	-> 先頭に黒が4連続ならば先頭に黒	... (1)
--
-- 29個で5個連続
--	-> 先頭に白
--	-> 先頭に黒が5連続でなければ先頭に黒	... (2)

-- 29個で4個連続で先頭に黒が4連続のもの
-- 	-> 24個で4連続以下
--
-- 29個で先頭に黒が5連続のもの
-- 	-> 23個で5連続以下

lesser :: Int -> Int -> Int
lesser n b = sum $ map (patterns n) [0 .. b]

patterns :: Int -> Int -> Int
patterns _ 0 = 1
patterns n b
	| n < b = 0
	| n == b = 1
patterns n b =
	lesser (n - b - 1) (b - 1) +
	2 * patterns (n - 1) b -
	lesser (n - b - 2) b

-- 6個で4個の場合
--
-- 5個で先頭に3個連続
--
-- 5個で4個連続
-- 	-> 先頭に4個連続でなければ黒白OK
-- 	-> 先頭に4個連続ならば白のみOK

main :: IO ()
main = print $ patterns 30 5
