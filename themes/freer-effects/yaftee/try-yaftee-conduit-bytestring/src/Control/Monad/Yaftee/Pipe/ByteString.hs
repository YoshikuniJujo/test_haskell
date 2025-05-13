{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString (
	hGet, hGet', putStr, hPutStr'
	) where

import Prelude hiding (putStr)
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bool
import Data.ByteString qualified as BS
import System.IO hiding (putStr)

hGet :: (U.Member Pipe.P es, U.Base (U.FromFirst IO) es) =>
	Int -> Handle -> Eff.E es i BS.ByteString ()
hGet bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>=
	bool (pure ()) (Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield >> go)

hGet' :: (U.Member Pipe.P es, U.Base (U.FromFirst IO) es) =>
	Int -> Handle -> Eff.E es i (Maybe BS.ByteString) ()
hGet' bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>= bool
	(Pipe.yield Nothing)
	(Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield . Just >> go)

putStr :: (U.Member Pipe.P es, U.Base IO.I es) => Eff.E es BS.ByteString o r
putStr = fix \go -> Pipe.await >>= Eff.effBase . BS.putStr >> go

hPutStr' h = fix \go -> do
	m <- Pipe.isMore
	if m
	then (>> go) $ Eff.effBase . BS.hPutStr h =<< Pipe.await
	else pure ()
