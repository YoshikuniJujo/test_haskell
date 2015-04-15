import Data.List
import Data.Function

main :: IO ()
main = interact $ unlines . sortBy (compare `on` toHuman) . lines

table :: [(Char, Integer)]
table = zip "bKMGTP" $ iterate (* 1024) 1

toHuman :: String -> Integer
toHuman s = case lookup (last s) table of
	Just u -> read (init s) * u
	_ -> read s
