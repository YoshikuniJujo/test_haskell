import Data.Char

main :: IO ()
main = interact camelToSnake

camelToSnake :: String -> String
camelToSnake (c : cs)
	| isUpper c = '_' : toLower c : camelToSnake cs
	| otherwise = c : camelToSnake cs
camelToSnake "" = ""
