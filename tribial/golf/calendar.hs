import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.WeekDate

main :: IO ()
main = interact $ unlines . ("Su Mo Tu We Th Fr Sa" :)
	. map (tail . concatMap show3)
	. (\[m, y] -> uncurry mkCal $ getData (read m) (read y))
	. words

getData :: Int -> Integer -> (Int, Int)
getData m y = let
	(_, _, dw) = toWeekDate $ fromGregorian y m 1
	ln = monthLength (isLeapYear y) m in
	(dw `mod` 7, ln)

mkCal :: Int -> Int -> [[Int]]
mkCal dw ln = sepN 7 $ replicate dw 0 ++ [1..ln]

sepN :: Int -> [a] -> [[a]]
sepN _ [] = []
sepN n xs = take n xs : sepN n (drop n xs)

show3 :: Int -> String
show3 0 = "   "
show3 x = let s = show x in replicate (3 - length s) ' ' ++ s
