{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SafeHandle where

import Data.IORef
import System.IO

newtype SafeHandle s = SafeHandle Handle deriving Show

withFileSafe :: FilePath -> IOMode -> (forall s . SafeHandle s -> IO a) -> IO a
withFileSafe fp md f = withFile fp md (f . SafeHandle)

hGetContentsSafe :: SafeHandle s -> IO String
hGetContentsSafe (SafeHandle h) = hGetContents h

sane :: IO ()
sane = withFileSafe "hello.txt" ReadMode \h ->
	putStr =<< hGetContentsSafe h

{-
insane :: IO ()
insane = do
	h <- withFileSafe "hello.txt" ReadMode pure
	putStr =<< hGetContentsSafe h

insane2 :: IO ()
insane2 = do
	handle <- newIORef undefined
	withFileSafe "hello.txt" ReadMode $ writeIORef handle
	putStr =<< hGetContentsSafe =<< readIORef handle
	-}
