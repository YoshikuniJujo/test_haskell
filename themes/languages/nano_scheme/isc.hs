import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import System.IO (stdout, hFlush)

import Value (Value, showValue, Error(..))
import Parse (parse, tokens)
import Eval (eval)
import Environment (Env, env0)

main :: IO ()
main = doWhile env0 $ \e -> do
	putStr "isc> " >> hFlush stdout
	src <- getLine
	case (`evaluate` e) =<< parse =<< tokens src of
		Right (vs, e') ->
			mapM_ (putStrLn . showValue) vs >> return (Just e')
		Left (Error em) -> putStrLn em >> return (Just e)
		Left Exit -> return Nothing

doWhile :: Monad m => s -> (s -> m (Maybe s)) -> m ()
doWhile s act = maybe (return ()) (`doWhile` act) =<< act s

evaluate :: [Value] -> Env -> Either Error ([Value], Env)
evaluate [] e = Right ([], e)
evaluate (v : vs) e = uncurry (<$>) . (first . (:) *** evaluate vs) =<< eval v e
