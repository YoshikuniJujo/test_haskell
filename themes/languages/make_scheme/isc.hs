import Control.Monad
import System.IO
import System.Environment
import System.Random

import InteractiveScheme

main :: IO ()
main = do
	as <- getArgs
	rg1 <- newStdGen
	rg2 <- newStdGen
	case as of
		[] -> run $ env0 rg1
		fp : _ -> do
			src <- readFile fp
			case load src $ env0 rg2 of
				Right (o, e') -> putStr o >> run e'
				Left e -> error $ show e

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
