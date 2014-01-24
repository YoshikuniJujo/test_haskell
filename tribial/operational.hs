{-# LANGUAGE GADTs, Rank2Types #-}

-- operational

type Program t = Free (Coyoneda t)

singleton :: t a -> Program t a
singleton = liftF . liftCoyoneda

interpret :: Monad m => (forall x . instr x -> m x) -> Program instr a -> m a
interpret eval (Free (Coyoneda t f)) = eval t >>= interpret eval . f
interpret _ (Pure a) = return a

-- Coyoneda

data Coyoneda t x where
	Coyoneda :: t r -> (r -> a) -> Coyoneda t a

instance Functor (Coyoneda t) where
	fmap f (Coyoneda t g) = Coyoneda t (f . g)

liftCoyoneda :: t a -> Coyoneda t a
liftCoyoneda t = Coyoneda t id

-- Free

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Monad (Free f) where
	return = Pure
	Pure a >>= k = k a
	Free fm >>= k = Free (fmap (>>= k) fm)

liftF :: Functor f => f a -> Free f a
liftF f = Free $ fmap Pure f

-- example

data CharIO x where
	GetCh :: CharIO Char
	PutCh :: Char -> CharIO ()
	LiftIO :: IO a -> CharIO a

type MyIO = Program CharIO

getCh :: Program CharIO Char
getCh = singleton GetCh

putCh :: Char -> Program CharIO ()
putCh = singleton . PutCh

liftIO :: IO a -> Program CharIO a
liftIO = singleton . LiftIO

runCharIO :: MyIO a -> IO a
runCharIO = interpret $ \io -> case io of
	GetCh -> getChar
	PutCh ch -> putChar ch
	LiftIO m -> m
