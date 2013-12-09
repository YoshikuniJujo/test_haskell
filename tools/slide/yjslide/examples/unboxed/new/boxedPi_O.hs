import Data.Word

times :: Word
times = 10 ^ 8

main :: IO ()
main = print $ 4 * getPi4 0 1 times

getPi4 :: Double -> Double -> Word -> Double
getPi4 p _ 0 = p
getPi4 p i n = getPi4 (p + recip i) (negate $ i + signum i * 2) (n - 1)
