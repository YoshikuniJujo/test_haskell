import Control.Applicative

example1, example2, example3, example4 :: [String]
example1 = ["+", "3", "4"]
example2 = ["*", "+", "1", "5", "+", "2", "3"]
example3 = ["/", "*", "+", "1", "5", "+", "2", "3", "4"]
example4 = ["/", "*", "+", "8", "5", "-", "2", "5", "4"]

type State = [Integer]

update :: String -> Maybe State -> Maybe State
update "+" (Just (x : y : ns)) = Just $ x + y : ns
update "*" (Just (x : y : ns)) = Just $ x * y : ns
update s ns = case reads s of
	(n, "") : _ -> (n :) <$> ns
	_ -> Nothing
