dist0 :: Double -> Double -> Double
dist0 x y = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)

dist0' :: (Double, Double) -> Double
dist0' (x, y) = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)

p :: (Double, Double)
p = (6.0, 4.0)
