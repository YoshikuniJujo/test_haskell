introduction :: (String, Integer) -> String
introduction (n, a) =
	"My name is " ++ n ++
	". I'm " ++ show a ++ " years old."

introductionYj :: Integer -> String
introductionYj = curry introduction "Yoshikuni Jujo"
