main :: IO ()
main =	getLine >>= \str ->
	getLine >>= \n ->
	putStrLn (concat $ replicate (read n) str) >>
	putStrLn (concat $ replicate (read n) (reverse str))

main' :: IO ()
main' = do
	str <- getLine
	n <- getLine
	putStrLn $ concat $ replicate (read n) str
	putStrLn $ concat $ replicate (read n) $ reverse str
