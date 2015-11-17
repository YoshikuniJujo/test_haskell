enqueue :: Int -> [Int] -> [Int]
enqueue = (:)

dequeue :: [Int] -> Maybe (Int, [Int])
dequeue q
	| null q = Nothing
	| otherwise = Just (last q, init q)
