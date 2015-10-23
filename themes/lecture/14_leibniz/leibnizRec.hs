
estPi :: Int -> Double
estPi = (4 *) . estPi4 . fromIntegral

estPi4 :: Double -> Double
estPi4 0 = 1
estPi4 n = estPi4 (n - 1) + (- 1) ** n / (2 * n + 1)
