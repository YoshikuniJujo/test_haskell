main :: IO ()
main = interact sep75

sep75 :: String -> String
sep75 "" = ""
sep75 s	| null d = t
	| otherwise = t ++ "\n" ++ sep75 d
	where
	(t, d) = splitAt 55 s
