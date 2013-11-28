some :: Int -> String
some 0 = ""
some n = "abc" ++ some (n - 1)

other :: Int -> String
other 0 = ""
other n = other (n - 1) ++ "abc"

main = do
	putStrLn $ some 20
	putStrLn $ other 20
