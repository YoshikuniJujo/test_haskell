
newtype StateIO s a = StateIO { runStateIO :: s -> IO (a, s) }

instance Monad (StateIO s) where
	return x = StateIO $ \s -> return (x, s)
	StateIO x >>= f = StateIO $ \s -> do
		(v, s') <- x s
		runStateIO (f v) s'

put :: s -> StateIO s ()
put x = StateIO $ \_ -> return ((), x)

get :: StateIO s s
get = StateIO $ \s -> return (s, s)

modify :: (s -> s) -> StateIO s ()
modify f = get >>= put . f

add :: Int -> StateIO Int ()
add x = do
	modify (+ x)
	StateIO $ \s -> do
		putStrLn $ "add " ++ show x
		return ((), s)

test :: StateIO Int Int
test = do
	add 8
	add 9
	s <- get
	add s
	get
