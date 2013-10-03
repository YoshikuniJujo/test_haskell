
main' :: IO ()
main' = do
	name <- getLine
	if name == "." then return () else do
		putStrLn $ "Hello, " ++ name ++ "!"
		main'

main'' :: IO ()
main'' = getLine >>= \name ->
	if name == "."
		then return ()
		else putStrLn ("Hello, " ++ name ++ "!") >>= \_ -> main

main :: IO ()
main = getLine >>= \name -> case name of
	"." -> return ()
	_ -> putStrLn ("Hello, " ++ name ++ "!") >>= \_ -> main
