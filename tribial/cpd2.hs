data Point = Cartesian Double Double | Polar Double Double deriving Show

mul :: Point -> Double -> Point
mul (Cartesian x y) n = Cartesian (x * n) (y * n)
mul (Polar d r) n = Polar (d * n) r

point1, point2 :: Point
point1 = Cartesian 8 5
point2 = Polar 6 (pi / 3)
