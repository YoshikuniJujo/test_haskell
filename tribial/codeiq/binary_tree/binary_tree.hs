import Control.Arrow

main :: IO ()
main = interact $ (++ "\n") . show
	. (\n -> if even n then 0 else trees $ n `div` 2) . read

cases :: [Integer]
cases = map trees [0 ..]

trees :: Int -> Integer
trees 0 = 1
trees n = sum
	[ (cases !! l) * (cases !! r) | l <- [0 .. n - 1], let r = n - l - 1 ]
