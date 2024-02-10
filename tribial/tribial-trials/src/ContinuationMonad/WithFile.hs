{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ContinuationMonad.WithFile where

import Control.Monad.Trans
import Control.Monad.Cont
import System.IO

foo :: IO ()
foo =	withFile "text/a" ReadMode \a ->
	withFile "text/b" ReadMode \b ->
	withFile "text/c" ReadMode \c ->
	withFile "text/d" ReadMode \d ->
	withFile "text/e" ReadMode \e ->
	withFile "text/f" ReadMode \f -> do
		putStr =<< hGetContents a
		putStr =<< hGetContents b
		putStr =<< hGetContents c
		putStr =<< hGetContents d
		putStr =<< hGetContents e
		putStr =<< hGetContents f

bar :: IO ()
bar = ($ pure) $ runContT do
	a <- ContT $ withFile "text/a" ReadMode
	b <- ContT $ withFile "text/b" ReadMode
	c <- ContT $ withFile "text/c" ReadMode
	d <- ContT $ withFile "text/d" ReadMode
	e <- ContT $ withFile "text/e" ReadMode
	f <- ContT $ withFile "text/f" ReadMode
	lift do	putStr =<< hGetContents a
		putStr =<< hGetContents b
		putStr =<< hGetContents c
		putStr =<< hGetContents d
		putStr =<< hGetContents e
		putStr =<< hGetContents f
