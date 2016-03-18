root :: Double -> [Double]
root 0 = [0]
root x	| x < 0 = []
	| otherwise = [- sqrt x, sqrt x]

-- root (root a + b)

calc :: Double -> Double -> [Double]
calc a b = root a >>= root . (+ b)

grd :: Bool -> [()]
grd False = []
grd _ = [()]

calc2, calc2' :: Double -> Double -> [Double]
calc2 a b = do
	x <- root a
	y <- root $ x + b
	grd $ y >= 0
	return y

calc2' a b = [ y | x <- root a, y <- root $ x + b, y >= 0 ]
