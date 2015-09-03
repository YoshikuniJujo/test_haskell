import Data.List

(.++), (.++.) :: [a] -> [a] -> [a]
(x : xs) .++ ys = x : xs .++ ys
[] .++ ys = ys

(.++.) = flip $ foldr (:)

(.++..) = curry . unfoldr $ \xys -> case xys of
	(x : xs, ys) -> Just (x, (xs, ys))
	(_, y : ys) -> Just (y, ([], ys))
	_ -> Nothing

concatRaw, concatF :: [[a]] -> [a]
concatRaw (xs : xss) = xs ++ concatRaw xss
concatRaw _ = []

concatF = foldr (++) []

reverseRaw, reverseF :: [a] -> [a]
reverseRaw = rv []
	where
	rv rs (x : xs) = rv (x : rs) xs
	rv rs _ = rs

reverseF = foldl (flip (:)) []
