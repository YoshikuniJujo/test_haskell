import System.IO

main :: IO ()
main = do
	getChar >>= print
	_ <- getLine
	hSetBuffering stdin NoBuffering
	getChar >>= print
