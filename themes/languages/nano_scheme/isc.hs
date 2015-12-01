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
	let rtn = (`eval` e) . fst =<< parse =<< tokens src
	case rtn of
		Right (v, e') -> putStrLn (showValue v) >> return (Just e')
		Left (Error em) -> putStrLn em >> return (Just e)
		Left Exit -> return Nothing

doWhile :: Monad m => s -> (s -> m (Maybe s)) -> m ()
doWhile s act = do
	rtn <- act s
	maybe (return ()) (`doWhile` act) rtn
