import Control.Applicative
import Data.List
import Text.Read

example1, example2, example3, example4 :: [String]
example1 = ["3", "4", "+"]
example2 = ["1", "5", "+", "2", "3", "+", "*"]
example3 = ["1", "5", "+", "2", "3", "+", "*", "4", "/"]
example4 = ["8", "5", "+", "2", "5", "-", "*", "4", "/"]

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

rpolish1 :: Maybe [Integer] -> String -> Maybe [Integer]
rpolish1 (Just ns) s = case lookup s operators of
	Just o -> case ns of
		y : x : ns' -> Just $ x `o` y : ns'
		_ -> Nothing
	_ -> maybe Nothing (Just . (: ns)) $ readMaybe s
rpolish1 _ _ = Nothing

rpolish :: [String] -> Maybe [Integer]
rpolish = foldl' rpolish1 $ Just []

rpolish1' :: [Integer] -> String -> Maybe [Integer]
rpolish1' (y : x : ns) s | Just o <- lookup s operators = Just $ x `o` y : ns
rpolish1' ns s = (: ns) <$> readMaybe s

rpolish' :: [String] -> Maybe [Integer]
rpolish' = foldl (flip $ (=<<) . flip rpolish1') $ Just []
