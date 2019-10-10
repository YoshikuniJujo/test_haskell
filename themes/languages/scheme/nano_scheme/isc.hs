{-
import Control.Applicative ((<$>))
import Control.Arrow (first, (***))

import Primitive (env0)
import Parse (parse, tokens)
import Eval (eval)
import Environment (Env, Value, showValue, Error(..))
-}

import System.IO (stdout, hFlush)

import NanoScheme

main :: IO ()
main = doWhile env0 $ \e -> do
	putStr "isc> " >> hFlush stdout
	src <- getLine
	case scheme src e of
		Right (vs, e') ->
			mapM_ (putStrLn . showValue) vs >> return (Just e')
		Left (Error em) -> putStrLn em >> return (Just e)
		Left Exit -> return Nothing

doWhile :: Monad m => s -> (s -> m (Maybe s)) -> m ()
doWhile s act = maybe (return ()) (`doWhile` act) =<< act s
