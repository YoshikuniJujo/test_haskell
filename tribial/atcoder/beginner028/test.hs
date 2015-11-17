main :: IO ()
main = interact $ (++ "\n") . check . read

check :: Int -> String
check n	| n < 60 = "Bad"
	| n < 90 = "Good"
	| n < 100 = "Great"
	| otherwise = "Perfect"
