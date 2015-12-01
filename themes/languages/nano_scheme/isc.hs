-- import Control.Applicative
import System.IO

import Value
import Parse
import Eval
import Environment

main :: IO ()
main = doWhile env0 $ \e -> do
	putStr "isc> "
	hFlush stdout
	src <- getLine
	let rtn = (`evaluate` e) =<< parse =<< tokens src
	case rtn of
		Right (vs, e') -> do
			mapM_ (putStrLn . showValue) vs
			return (Just e')
		Left (Error em) -> putStrLn em >> return (Just e)
		Left Exit -> return Nothing

doWhile :: Monad m => s -> (s -> m (Maybe s)) -> m ()
doWhile s act = do
	rtn <- act s
	maybe (return ()) (`doWhile` act) rtn

evaluate :: [Value] -> Env -> Either Error ([Value], Env)
evaluate [] e = Right ([], e)
evaluate (v : vs) e = do
	(v', e') <- eval v e
	(vs', e'') <- evaluate vs e'
	return (v' : vs', e'')
