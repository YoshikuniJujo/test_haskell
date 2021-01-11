hasGoneAt :: Integer -> Integer -> Bool
0 `hasGoneAt` 1 = True
y1 `hasGoneAt` y2 = (y1 - 1) `hasGoneAt` (y2 - 1)

y2020HasGone :: Bool
y2020HasGone = 2020 `hasGoneAt` 2021
