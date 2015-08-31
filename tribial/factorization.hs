import Data.List

popFactor :: Integer -> Maybe (Integer, Integer)
popFactor 1 = Nothing
popFactor n = Just (f, n `div` f)
	where f = head $ filter ((== 0) . (n `mod`)) [2 ..]

factorization, factorization' :: Integer -> [Integer]
factorization n = case popFactor n of
	Nothing -> []
	Just (f, n') -> f : factorization n'

factorization' = unfoldr popFactor
