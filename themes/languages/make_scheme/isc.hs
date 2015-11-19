import Control.Monad
import System.IO

import InteractiveScheme

main :: IO ()
main = run env0

run :: Env -> IO ()
run e = do
	putStr "isc> "
	hFlush stdout
	l <- getLine
	case scheme l e of
		Left Exit -> return ()
		Left (Error msg) -> putStrLn msg >> run e
		Right (r, e') -> putStrLn r >> run e'

doWhile :: IO Bool -> IO ()
doWhile a = a >>= (`when` doWhile a)
