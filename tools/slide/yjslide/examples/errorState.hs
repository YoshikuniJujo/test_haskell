{-# LANGUAGE PackageImports, TypeFamilies #-}

import "monads-tf" Control.Monad.State hiding (put, get, modify)
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.Identity

type ErrorState = ErrorT String (State Int)

type StateError = StateT Int (ErrorT String Identity)

data StateMaybe s a = StateMaybe { runStateMaybe :: s -> Maybe (a, s) }

instance Monad (StateMaybe s) where
	return a = StateMaybe $ \s -> Just (a, s)
	StateMaybe x >>= f = StateMaybe $ \s -> case x s of
		Just (v, s') -> runStateMaybe (f v) s'
		_ -> Nothing

put :: s -> StateMaybe s ()
put s = StateMaybe $ \_ -> Just ((), s)

get :: StateMaybe s s
get = StateMaybe $ \s -> Just (s, s)

modify :: (s -> s) -> StateMaybe s ()
modify f = get >>= put . f

nothing :: StateMaybe s a
nothing = StateMaybe $ \_ -> Nothing

type ExamMonad = StateMaybe Int

addMemory :: Int -> ExamMonad ()
addMemory n = modify (+ n)

subMemory :: Int -> ExamMonad ()
subMemory n = modify (subtract n) >> checkMemory

checkMemory :: ExamMonad ()
checkMemory = do
	s <- get
	when (s < 0) $ nothing

subAll :: Int -> [Int] -> StateMaybe Int Int
subAll n ss = do
	addMemory n
	mapM subMemory ss
	get
