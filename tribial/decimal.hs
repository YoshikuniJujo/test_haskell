import Data.List

decimal :: Integer -> [Integer]
decimal = unfoldr $ \n -> case n of
	0 -> Nothing
	_ -> Just (n `mod` 10, n `div` 10)
