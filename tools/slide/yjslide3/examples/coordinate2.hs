type Rect = (Double, Double)

mulRect :: Rect -> Double -> Rect
mulRect (x, y) n = (x * n, y * n)
