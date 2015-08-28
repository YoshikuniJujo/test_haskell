import Data.List
import Data.Maybe

main :: IO ()
main = interact $ show . toInt . head . lines

digits :: [Char]
digits = ['零', '一', '二', '三', '四', '五', '六', '七', '八', '九', '十']

toInt :: String -> Int
toInt (d1 : '十' : [d2]) = fromJust (elemIndex d1 digits) * 10
	+ fromJust (elemIndex d2 digits)
toInt (d1 : "十") = fromJust (elemIndex d1 digits) * 10
toInt ([d1]) = fromJust (elemIndex d1 digits)
