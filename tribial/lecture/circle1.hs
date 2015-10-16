data Circle = Circle Double Double Double deriving Show

area :: Circle -> Double
area (Circle _ _ r) = r ^ 2 * pi

inside :: Circle -> (Double, Double) -> Bool
inside (Circle cx cy r) (x, y) = (x - cx) ^ 2 + (y - cy) ^ 2 <= r ^ 2

moveH :: Circle -> Double -> Circle
moveH (Circle cx cy r) dx = Circle (cx + dx) cy r
