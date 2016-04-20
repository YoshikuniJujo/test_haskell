import System.Environment

main :: IO ()
main = do
	args <- getArgs
	case args of
		[[c], [d], fp] -> readFile fp >>= putStr . map (tr c d)
		_ -> putStrLn "Usage tr c d file.txt"

tr :: Eq a => a -> a -> a -> a
tr c d x | x == c = d | otherwise = x
