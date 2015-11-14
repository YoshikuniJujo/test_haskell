
testE :: Int -> Int -> Bool
testE n i = 2 * (n `mod` i) >= i

checkE :: Int -> [Int]
checkE n = filter (testE n) [1 .. n]

countE :: Int -> Int
countE = length . checkE

testO :: Int -> Int -> Bool
testO n i = n `mod` i == 0

checkO :: Int -> [Int]
checkO n = filter (testO n) [1 .. n]

countO :: Int -> Int
countO = length . checkO

bs :: [Int]
bs = map (count . (`div` 4) . (^ 2)) [0 ..]

counts :: [Int]
counts = map count [0 ..]

count :: Int -> Int
count 0 = 0
count n	| even n = n `div` 2 + countE (n `div` 2) + 2 * counts !! (n `div` 2)
	| otherwise = countO n + counts !! (n - 1)
