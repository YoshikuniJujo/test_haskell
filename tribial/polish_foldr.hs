import Control.Applicative
import Text.Read

example1, example2, example3, example4 :: [String]
example1 = ["+", "3", "4"]
example2 = ["*", "+", "1", "5", "+", "2", "3"]
example3 = ["/", "*", "+", "1", "5", "+", "2", "3", "4"]
example4 = ["/", "*", "+", "8", "5", "-", "2", "5", "4"]

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish1 :: String -> Maybe [Integer] -> Maybe [Integer]
polish1 s (Just ns) = case lookup s operators of
	Just o -> case ns of
		x : y : ns -> Just $ x `o` y : ns
		_ -> Nothing
	_ -> maybe Nothing (Just . (: ns)) $ readMaybe s
polish1 _ _ = Nothing

polish :: [String] -> Maybe [Integer]
polish = foldr polish1 $ Just []

polish1' :: String -> [Integer] -> Maybe [Integer]
polish1' s (x : y : ns) | Just o <- lookup s operators = Just $ x `o` y : ns
polish1' s ns = (: ns) <$> readMaybe s

polish' :: [String] -> Maybe [Integer]
polish' = foldr ((=<<) . polish1') $ Just []
