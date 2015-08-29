
myEnumFromTo :: Integer -> Integer -> [Integer]
myEnumFromTo m n | m > n = []
myEnumFromTo m n = m : myEnumFromTo (m + 1) n

myEnumFromTo' :: Integer -> Integer -> [Integer]
myEnumFromTo' m n
	| m > n = []
	| otherwise = m : myEnumFromTo' (m + 1) n
