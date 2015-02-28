import Data.Char

checkAnswer :: Char -> Maybe Bool
checkAnswer c = case toLower c of
	'y' -> Just True
	'n' -> Just False
	_ -> Nothing
