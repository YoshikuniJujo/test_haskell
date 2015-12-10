main :: IO ()
main = interact $ show . count . read

count :: Int -> Int
count 1 = 0
count n = (+ 1) . count . ($ n) $ if even n then (`div` 2) else subtract 1
