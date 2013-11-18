import System.Directory

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Monad MaybeIO where
	return = MaybeIO . return . Just
	MaybeIO io >>= f = MaybeIO $ do
		mx <- io
		case mx of
			Just x -> runMaybeIO $ f x
			_ -> return Nothing

nothing :: MaybeIO a
nothing = MaybeIO $ return Nothing

lift :: IO a -> MaybeIO a
lift io = MaybeIO $ io >>= return . Just

maybeReadFile :: FilePath -> MaybeIO String
maybeReadFile fp = do
	ex <- lift $ doesFileExist fp
	if ex then lift $ readFile fp else nothing

test :: MaybeIO ()
test = do
	fp <- lift getLine
	cnt <- maybeReadFile fp
	lift $ putStr cnt
