import Data.List (unfoldr)
import Prelude hiding (filter)

filter :: (a -> Bool) -> [a] -> [a]
filter p = unfoldr $ \lst -> case dropWhile (not . p) lst of
	[] -> Nothing
	x : xs -> Just (x, xs)
