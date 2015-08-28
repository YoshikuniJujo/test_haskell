newtype R a = R { unR :: R a -> a }

f :: R a -> a
f x = unR x x

-- g y = y y
