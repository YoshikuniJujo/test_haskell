pyth x y = sqrt $ (x * x) + (y * y)

pyth' x y k = mul x x $ \x2 ->
	mul y y $ \y2 ->
		add x2 y2 $ \x2y2 -> sqrt' x2y2 k

mul :: Double -> Double -> (Double -> Double) -> Double
mul x y k = k $ x * y

add :: Double -> Double -> (Double -> Double) -> Double
add x y k = k $ x + y

sqrt' :: Double -> (Double -> Double) -> Double
sqrt' x k = k $ sqrt x
