import Control.Applicative
import Control.Monad
import Data.Char

newtype CPS r a = CPS { runCPS :: (a -> r) -> r }

value :: CPS a a -> a
value (CPS c) = c id

instance Functor (CPS r) where
	fmap f (CPS c) = CPS $ \g -> c $ g . f

instance Applicative (CPS r) where
	pure x = CPS ($ x)
	CPS x <*> CPS y = CPS $ x . (y .) . (.)

instance Monad (CPS r) where
	return = pure
	CPS c >>= f = CPS $ c . (. f) . flip runCPS

callCC :: ((a -> CPS r b) -> CPS r a) -> CPS r a
callCC f = CPS $ runCPS <$> f . ((CPS . const) .) <*> id

fun :: Int -> String
fun n = (`runCPS` id) $ do
	str <- callCC $ \exit1 -> do
		when (n < 10) $ exit1 (show n)
		let ns = map digitToInt $ show (n `div` 2)
		n' <- callCC $ \exit2 -> do
			when (length ns < 3) $ exit2 (length ns)
			when (length ns < 5) $ exit2 n
			when (length ns < 7) $ do
				let ns' = map intToDigit $ reverse ns
				exit1 $ dropWhile (== '0') ns'
			return $ sum ns
		return $ "(ns = " ++ show ns ++ ") " ++ show n'
	return $ "Answer: " ++ str
