import Data.List

filterU p = unfoldr $ \l -> case dropWhile (not . p) l of
	x : xs -> Just (x, xs)
	_ -> Nothing
