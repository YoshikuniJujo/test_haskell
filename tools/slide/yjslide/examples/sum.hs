
main :: IO ()
main = getSum >>= print

getSum :: IO Integer
getSum = getLine >>= \input -> case input of
	"." -> return 0
	_ -> getSum >>= return . (read input +)

getSum' = do
	input <- getLine
	case input of
		"." -> return 0
		_ -> do	s <- getSum
			return $ read input + s
