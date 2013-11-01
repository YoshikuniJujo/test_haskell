{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Cont
import Control.Monad

testLoop :: Int -> Cont r Int
testLoop n = do
	callCC $ \break -> do
		when (n > 8) $ break n
		callCC $ \continue -> do
			when (n < 0) $ continue ()
			when (n < 3) $ break n
		testLoop (n + 1)

loop :: a -> (a -> a) ->
	(a -> (a -> Cont r b) -> (c -> Cont r d) -> Cont r c) -> Cont r a
loop x0 f act = do
	callCC $ \break -> do
		callCC $ \continue -> do
			act x0 break continue
		loop (f x0) f act

some :: Int -> Cont r Int
some n = loop n (+ 1) $ \x break continue -> do
	when (x > 8) $ break x
	when (x < 0) $ continue ()
	when (x < 3) $ break x
