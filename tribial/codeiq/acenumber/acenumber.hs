import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . f . read

f :: Integral n => n -> n
f n	| n' < 0 = sum' $ take (fromIntegral n) bs
	| otherwise = modulo $ sum' bs
		+ sum' rs `mul` (n' `div` fromIntegral m)
		+ sum' (take (fromIntegral n' `mod` m) rs)
	where (bs, rs) = reps; m = length rs; n' = n - fromIntegral (length bs)

reps :: Integral n => ([n], [n])
reps = period [] $ iterate (modulo . (* 16)) 0xA

period :: Eq n => [n] -> [n] -> ([n], [n])
period ps (x : xs)
	| Just p <- x `elemIndex` ps = splitAt (length ps - p - 1) $ reverse ps
	| otherwise = period (x : ps) xs

sum' :: Integral n => [n] -> n
sum' = foldl' ((modulo .) . (+)) 0

mul :: Integral n => n -> n -> n
mul = (modulo .) . (*)

modulo :: Integral n => n -> n
modulo = (`mod` 10 ^ (6 :: Int))
