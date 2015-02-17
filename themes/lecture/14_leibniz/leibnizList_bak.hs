estPi :: Int -> Double
estPi = (4 *) . estPi4

estPi' :: Int -> Double
estPi' = (4 *) . estPi4'

estPi'' :: Int -> Double
estPi'' = (4 *) . estPi4''

estPi''' :: Int -> Double
estPi''' = (4 *) . estPi4'''

estPi'''' :: Int -> Double
estPi'''' = (4 *) . estPi4''''

estPi4 :: Int -> Double
estPi4 = sum . (`take` map (\k -> (- 1) ** k / (2 * k + 1)) [0 ..])

estPi4' :: Int -> Double
estPi4' = foldl (+) 0 . (`take` map (\k -> (- 1) ** k / (2 * k + 1)) [0 ..])

estPi4'' :: Int -> Double
estPi4'' = foldl (+) 1 . tail . (`take` map (\k -> (- 1) ** k / (2 * k + 1)) [0 ..])

estPi4''' :: Int -> Double
estPi4''' = foldr (+) 0 . (`take` map (\k -> (- 1) ** k / (2 * k + 1)) [0 ..])

estPi4'''' :: Int -> Double
estPi4'''' = foldr (+) 1 . tail . (`take` map (\k -> (- 1) ** k / (2 * k + 1)) [0 ..])
