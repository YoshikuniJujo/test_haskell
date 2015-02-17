estPi :: Int -> Double
estPi = (4 *) . estPi4

estPi4 :: Int -> Double
estPi4 = sum . (`take` map (\k -> (- 1) ** k / (2 * k + 1)) [0 ..])
