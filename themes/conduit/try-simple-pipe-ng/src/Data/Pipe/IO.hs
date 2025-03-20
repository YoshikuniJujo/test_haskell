{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}

module Data.Pipe.IO (fromHandle, toHandle, fromFile, toFile, debug) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Exception (catch)
import Data.Pipe
import System.IO
import GHC.IO.Exception

fromHandle :: (PipeClass p, MonadBase IO m,
	MonadTrans (p i Char), Monad (p i Char m)) => Handle -> p i Char m ()
fromHandle h = do
	mc <- lift . liftBase $ (Just <$> hGetChar h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandle h) . yield) mc

toHandle :: (PipeClass p, MonadBase IO m,
	MonadTrans (p Char o), Monad (p Char o m)) => Handle -> p Char o m ()
toHandle h = await >>= maybe (return ())
	((>> toHandle h) . lift . liftBase . hPutChar h)

fromFile :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p i Char), Monad (p i Char m)) => FilePath -> p i Char m ()
fromFile fp =
	bracket (liftBase $ openFile fp ReadMode) (liftBase . hClose) fromHandle

toFile :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p Char o), Monad (p Char o m)) => FilePath -> p Char o m ()
toFile fp = bracket (liftBase $ openFile fp WriteMode) (liftBase . hClose) toHandle

debug :: (PipeClass p, MonadBase IO m,
	MonadTrans (p a a), Monad (p a a m), Show a) => p a a m ()
debug = await >>= maybe (return ())
	(\x -> lift (liftBase $ print x) >> yield x >> debug)
