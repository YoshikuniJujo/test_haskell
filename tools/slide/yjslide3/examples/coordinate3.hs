data Rect = Rect Double Double deriving Show
data Pol = Pol Double Double deriving Show

mulRect :: Rect -> Double -> Rect
mulRect (Rect x y) n = Rect (x * n) (y * n)
