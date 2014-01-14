square :: Int -> Int
square x = x ^ 2

squareAll :: [Int] -> [Int]
squareAll = map (^ 2)

squareSum :: Int -> Int
squareSum n = sum $ map (^ 2) [0 .. n]

lucky :: Int -> Bool
lucky 4 = False
lucky 9 = False
lucky _ = True

squareSum' :: Int -> Int
squareSum' n = sum $ map (^ 2) $ filter lucky [0 .. n]

squareSum'' :: Int -> Int
squareSum'' n = sum $ filter lucky $ map (^ 2) $ filter lucky [0 .. n]
