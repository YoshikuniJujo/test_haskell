import Data.Char

readInt :: String -> Either Int String
readInt str
	| all isDigit str = Left $ read str
	| otherwise = Right str
