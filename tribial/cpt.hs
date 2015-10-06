type Cartesian = (Double, Double)

mulC :: Cartesian -> Double -> Cartesian
mulC (x, y) n = (x * n, y * n)

point1 :: Cartesian
point1 = (8, 5)

type Polar = (Double, Double)

mulP :: Polar -> Double -> Polar
mulP (d, r) n = (d * n, r)

point2 :: Polar
point2 = (6, pi / 3)
