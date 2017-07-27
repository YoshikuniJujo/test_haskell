a = Just 3

getIntMaybe :: Maybe Int -> Int
getIntMaybe mv = case mv of
	Just x -> x + 3
	Nothing -> 16

main = print (getIntMaybe a)
