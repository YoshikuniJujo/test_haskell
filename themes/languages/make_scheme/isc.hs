import Control.Applicative
import Control.Monad
import Data.Time
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
		[] -> run False $ env0 rg1
		"--time" : fp : _ -> repl True fp rg2
		fp : _ -> repl False fp rg2

repl :: Bool -> FilePath -> StdGen -> IO ()
repl t fp rg = do
	src <- readFile fp
	case load src $ env0 rg of
		Right (o, e') -> putStr o >> run t e'
		Left e -> error $ show e

run :: Bool -> Env -> IO ()
run t e = do
	putStr "isc> "
	hFlush stdout
	l <- getLine
	t0 <- getCurrentTime
	case scheme l e of
		Left Exit -> return ()
		Left (Error msg) -> putStrLn msg >> run t e
		Right (r, e') -> do
			putStrLn r
			when t $ print =<< (`diffUTCTime` t0) <$> getCurrentTime
			run t e'

doWhile :: IO Bool -> IO ()
doWhile a = a >>= (`when` doWhile a)
