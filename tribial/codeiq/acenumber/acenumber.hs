import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . f . read

as :: Integral n => [n]
as = iterate (modulo . (* 16)) 0xA

modulo :: Integral n => n -> n
modulo = (`mod` 10 ^ 6)

f :: Integral n => n -> n
f n	| n' < 0 = modulo . sum' $ take (fromIntegral n) xs
	| otherwise = modulo $ sum' xs
		+ sum' ys `mul` (n' `div` m)
		+ sum' (take (fromIntegral $ n' `mod` m) ys)
	where
	sum' = foldl' ((modulo .) . (+)) 0
	mul = (modulo .) . (*)
	(xs, ys) = period [] as
	m = fromIntegral $ length ys
	n' = n - fromIntegral (length xs)

period :: Integral n => [n] -> [n] -> ([n], [n])
period ps (x : xs)
	| Just p <- x `elemIndex` ps = splitAt (length ps - p - 1) $ reverse ps
	| otherwise = period (x : ps) xs
