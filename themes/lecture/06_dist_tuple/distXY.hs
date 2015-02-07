dist0 :: Double -> Double -> Double
dist0 x y = sqrt $ x ^ 2 + y ^ 2

px, py :: Double
px = 9.0
py = 5.0

p :: (Double, Double)
p = (9.0, 5.0)

dist0' :: (Double, Double) -> Double
dist0' (x, y) = sqrt $ x ^ 2 + y ^ 2
