import System.Directory

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
	return = MaybeT . return . Just
	MaybeT m >>= f = MaybeT $ do
		v <- m
		case v of
			Just x -> runMaybeT $ f x
			_ -> return Nothing

lift :: Monad m => m a -> MaybeT m a
lift m = MaybeT $ do
	v <- m
	return $ Just v

nothing :: Monad m => MaybeT m a
nothing = MaybeT $ return Nothing

maybeReadFile :: FilePath -> MaybeT IO String
maybeReadFile fp = do
	ex <- lift $ doesFileExist fp
	if ex then lift $ readFile fp else nothing
