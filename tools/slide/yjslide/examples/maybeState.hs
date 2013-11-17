import Control.Monad

newtype MaybeState s a = MaybeState { runMaybeState :: s -> (Maybe a, s) }

instance Monad (MaybeState s) where
	return x = MaybeState $ \s -> (Just x, s)
	MaybeState x >>= f = MaybeState $ \s -> case x s of
		(Just v, s') -> runMaybeState (f v) s'
		(Nothing, s') -> (Nothing, s')

put :: s -> MaybeState s ()
put s = MaybeState $ \_ -> (Just (), s)

get :: MaybeState s s
get = MaybeState $ \s -> (Just s, s)

modify :: (s -> s) -> MaybeState s ()
modify f = get >>= put . f

nothing :: MaybeState s a
nothing = MaybeState $ \s -> (Nothing, s)

type ExamMonad = MaybeState Int

addMemory :: Int -> ExamMonad ()
addMemory n = modify (+ n)

subMemory :: Int -> ExamMonad ()
subMemory n = modify (subtract n) >> checkMemory

checkMemory :: ExamMonad ()
checkMemory = do
	s <- get
	when (s < 0) nothing

subAll :: Int -> [Int] -> MaybeState Int Int
subAll n ss = do
	addMemory n
	mapM subMemory ss
	get
