import Data.List
import Data.Maybe

makeTriangle :: Int -> Int -> Int -> Maybe Int
makeTriangle a b c
	| c < a + b = Just $ a + b + c
	| otherwise = Nothing

sorted_bigTriangle :: [Int] -> Maybe Int
sorted_bigTriangle [_, _] = Nothing
sorted_bigTriangle (c : b : a : xs) = case makeTriangle a b c of
	Just d -> Just d
	Nothing -> sorted_bigTriangle (b : a : xs)

bigTriangle :: [Int] -> Int
bigTriangle = fromMaybe 0 . sorted_bigTriangle . sortBy (flip compare)
