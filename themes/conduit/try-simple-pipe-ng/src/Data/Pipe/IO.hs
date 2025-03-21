{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Pipe.IO (

-- * READ AND WRITE CHAR

fromHandleChar, toHandleChar, fromFileChar, toFileChar,

-- * PRINT FLOWING DATA TO DEBUG

debug

) where

import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Exception (catch)
import Data.Pipe
import System.IO
import GHC.IO.Exception

fromHandleChar :: (PipeClass p, MonadBase IO m,
	MonadTrans (p i Char), Monad (p i Char m)) => Handle -> p i Char m ()
fromHandleChar h = do
	mc <- lift . liftBase $ (Just <$> hGetChar h) `catch` \(e :: IOException) ->
		case ioe_type e of
			EOF -> return Nothing
			_ -> ioException e
	maybe (return ()) ((>> fromHandleChar h) . yield) mc

toHandleChar :: (PipeClass p, MonadBase IO m,
	MonadTrans (p Char o), Monad (p Char o m)) => Handle -> p Char o m ()
toHandleChar h = await >>= maybe (return ())
	((>> toHandleChar h) . lift . liftBase . hPutChar h)

fromFileChar :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p i Char), Monad (p i Char m)) => FilePath -> p i Char m ()
fromFileChar fp =
	bracket (liftBase $ openFile fp ReadMode) (liftBase . hClose) fromHandleChar

toFileChar :: (PipeClass p, MonadBaseControl IO m,
	MonadTrans (p Char o), Monad (p Char o m)) => FilePath -> p Char o m ()
toFileChar fp = bracket (liftBase $ openFile fp WriteMode) (liftBase . hClose) toHandleChar

debug :: (PipeClass p, MonadBase IO m,
	MonadTrans (p a a), Monad (p a a m), Show a) => p a a m ()
debug = await >>= maybe (return ())
	(\x -> lift (liftBase $ print x) >> yield x >> debug)
