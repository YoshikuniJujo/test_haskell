all1To9 :: String -> Bool
all1To9 str = any (\c -> all (== c) str) "123456789"

sumZorome :: Int -> Int
sumZorome n = sum $ take n $ filter (all1To9 . show) [10 ..]
