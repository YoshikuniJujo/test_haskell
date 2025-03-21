{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Pipe
import Data.Char
import System.IO
import Control.Monad.Trans

main :: IO ()
main = void $ runPipe $ readFileP "files/sample.txt"
	=$= takeP 3
	=$= convert (map toUpper)
	=$= writeString

readFileP :: FilePath -> Pipe () String IO ()
readFileP fp = bracket (openFile fp ReadMode) hClose hRead

hRead :: Handle -> Pipe () String IO ()
hRead h = do
	eof <- lift $ hIsEOF h
	if eof then return () else do
		l <- lift $ hGetLine h
		yield l
		hRead h

writeString :: Pipe String () IO ()
writeString = do
	ms <- await
	case ms of
		Just s -> lift (putStrLn s) >> writeString
		_ -> return ()

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()
