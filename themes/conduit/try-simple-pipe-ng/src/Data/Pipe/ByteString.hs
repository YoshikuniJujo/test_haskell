{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}

module Data.Pipe.ByteString (
	fromHandle, toHandle, fromFile, toFile,
	fromHandleLn, toHandleLn, fromFileLn, toFileLn,
	toLazy) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Exception (catch)
import Data.Pipe
import qualified Data.Pipe.Lazy as L
import System.IO
import GHC.IO.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

bufferSize :: Int
bufferSize = 65536

fromHandleLn :: (PipeClass p, MonadBase IO m,
	MonadTrans (p i BSC.ByteString), Monad (p i BSC.ByteString m)) =>
	Handle -> p i BSC.ByteString m ()
fromHandleLn h = do
	ml <- lift . liftBase $
		(Just <$> BSC.hGetLine h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandleLn h) . yield) ml

toHandleLn :: (PipeClass p, MonadBase IO m,
	MonadTrans (p BSC.ByteString o), Monad (p BSC.ByteString o m)) =>
	Handle -> p BSC.ByteString o m ()
toHandleLn h = await >>= maybe (return ())
	((>> toHandleLn h) . lift . liftBase . BSC.hPutStrLn h)

fromFileLn :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p i BSC.ByteString), Monad (p i BSC.ByteString m)) =>
	FilePath -> p i BSC.ByteString m ()
fromFileLn fp =
	bracket (liftBase $ openFile fp ReadMode) (liftBase . hClose) fromHandleLn

toFileLn :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p BSC.ByteString o), Monad (p BSC.ByteString o m)) =>
--	MonadIO (p BSC.ByteString o m)) =>
	FilePath -> p BSC.ByteString o m ()
toFileLn fp = bracket
	(liftBase $ openFile fp WriteMode)
	(liftBase . hClose) toHandleLn

fromHandle :: (PipeClass p, MonadBase IO m,
	MonadTrans (p i BSC.ByteString), Monad (p i BSC.ByteString m)) =>
	Handle -> p i BSC.ByteString m ()
fromHandle h = do
	eof <- lift . liftBase $ hIsEOF h
	if eof then return () else do
		bs <- lift . liftBase $ BS.hGetSome h bufferSize
		yield bs
		fromHandle h

toHandle :: (PipeClass p, MonadBase IO m,
	MonadTrans (p BSC.ByteString o), Monad (p BSC.ByteString o m)) =>
	Handle -> p BSC.ByteString o m ()
toHandle h = await >>= maybe (return ())
	((>> toHandle h) . lift . liftBase . BSC.hPut h)

fromFile :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p i BSC.ByteString), Monad (p i BSC.ByteString m)) =>
	FilePath -> p i BSC.ByteString m ()
fromFile fp =
	bracket (liftBase $ openFile fp ReadMode) (liftBase . hClose) fromHandle

toFile :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p BSC.ByteString o), Monad (p BSC.ByteString o m)) =>
	FilePath -> p BSC.ByteString o m ()
toFile fp = bracket
	(liftBase $ openFile fp WriteMode)
	(liftBase . hClose) toHandle

toLazy :: (PipeClass p, L.PipeLazy p, MonadBaseControl IO m) =>
	p i BSC.ByteString m r -> m LBS.ByteString
toLazy = (LBS.fromChunks <$>) . L.toLazy
