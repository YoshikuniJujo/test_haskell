import Text.Read

example1, example2, example3, example4 :: [String]
example1 = ["3", "4", "+"]
example2 = ["1", "5", "+", "2", "3", "+", "*"]
example3 = ["1", "5", "+", "2", "3", "+", "*", "4", "/"]
example4 = ["8", "5", "+", "2", "5", "-", "*", "4", "/"]

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

rpolishIter :: Maybe [Integer] -> [String] -> Maybe [Integer]
rpolishIter mns [] = mns
rpolishIter (Just ns) (s : ss) = case lookup s operators of
	Just o -> case ns of
		y : x : ns' -> rpolishIter (Just $ x `o` y : ns') ss
		_ -> Nothing
	_ -> rpolishIter (maybe Nothing (Just . (: ns)) $ readMaybe s) ss
rpolishIter _ _ = Nothing

rpolish :: [String] -> Maybe [Integer]
rpolish = rpolishIter $ Just []
