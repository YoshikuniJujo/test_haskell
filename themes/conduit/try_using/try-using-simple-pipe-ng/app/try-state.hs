{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Base
import Data.Pipe
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import System.IO
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.State

main :: IO ()
main = void . (`runStateT` "") . runPipe $ readFileP "files/sample.txt"
	=$= takeByteString 7
	=$= takeP 6
	=$= writeString

readFileP :: MonadBaseControl IO m => FilePath -> Pipe () BS.ByteString m ()
readFileP fp = bracket (liftBase $ openFile fp ReadMode) (liftBase . hClose) hRead

hRead :: MonadBase IO m => Handle -> Pipe () BS.ByteString m ()
hRead h = do
	eof <- lift . liftBase $ hIsEOF h
	if eof then return () else do
		l <- lift . liftBase $ BSC.hGetLine h
		yield l
		hRead h

writeString :: MonadBase IO m => Pipe BS.ByteString () m ()
writeString = do
	ms <- await
	case ms of
		Just s -> lift (liftBase $ BSC.putStrLn s) >> writeString
		_ -> return ()

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()

takeByteString :: (MonadState m, StateType m ~ BS.ByteString) =>
	Int -> Pipe BS.ByteString BS.ByteString m ()
takeByteString n = do
	obs <- get
	let	ln = BS.length obs
	if ln >= n
	then yield (BS.take n obs) >> modify (BS.drop n) >> takeByteString n
	else do	mbs <- await
		case mbs of
			Just nbs -> do
				yield $ obs `BS.append` BS.take (n - ln) nbs
				put $ BS.drop (n - ln) nbs
				takeByteString n
			Nothing -> yield obs >> pure ()
