data Cartesian = Cartesian Double Double deriving Show

mulC :: Cartesian -> Double -> Cartesian
mulC (Cartesian x y) n = Cartesian (x * n) (y * n)

point1 :: Cartesian
point1 = Cartesian 8 5

data Polar = Polar Double Double deriving Show

mulP :: Polar -> Double -> Polar
mulP (Polar d r) n = Polar (d * n) r

point2 :: Polar
point2 = Polar 6 (pi / 3)
