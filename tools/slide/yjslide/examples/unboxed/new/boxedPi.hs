import Data.Word

times :: Word
times = 10 ^ 8
-- times = 10 ^ 7

main :: IO ()
main = print $ 4 * getPi4 0 1 times

getPi4 :: Double -> Double -> Word -> Double
getPi4 p _ 0 = p
-- getPi4 p i n = getPi4 (p + recip i) (negate $ i + signum i * 2) (n - 1)
getPi4 p i n = let
	p' = p + recip i
	i' = negate $ i + signum i * 2
	in p' `seq` i' `seq` getPi4 p' i' (n - 1)
{-
getPi4 p i n =
	(p + recip i) `seq` (negate $ i + signum i * 2) `seq`
	getPi4 (p + recip i) (negate $ i + signum i * 2) (n - 1)
	-}
