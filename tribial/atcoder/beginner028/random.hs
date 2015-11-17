import Numeric

main :: IO ()
main = interact $ (++ "\n") . (\d -> showFFloat Nothing d "")
	. (\[n, k] -> f n k) . map read . words

f :: Double -> Double -> Double
f n k = (1 + 3 * (n - 1) + 6 * (n - k) * (k - 1)) / n ^ 3
