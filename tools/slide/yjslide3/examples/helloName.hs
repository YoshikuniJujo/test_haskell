main :: IO ()
main = do
	putStrLn "What's your name?"
	str <- getLine
	if null str
		then return ()
		else do	putStrLn $ "Hello, " ++ str ++ "!"
			main
