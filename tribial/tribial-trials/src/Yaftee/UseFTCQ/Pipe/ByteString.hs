{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.ByteString where

import Control.Monad.Fix
import Data.ByteString qualified as BS
import Data.Bool
import System.IO

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.IO qualified as IO
import Yaftee.OpenUnion qualified as Union

hGet bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>=
	bool (pure ()) (Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield >> go)

hGet' bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>= bool
	(Pipe.yield Nothing)
	(Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield . Just >> go)

putStr :: (
	Union.Member Pipe.P effs,
	Union.Base IO.I effs ) => Eff.E effs BS.ByteString o r
putStr = fix \go -> Pipe.await >>= Eff.effBase . BS.putStr >> go

hPutStr :: (
	Union.Member Pipe.P es,
	Union.Base IO.I es ) =>
	Handle -> Eff.E es BS.ByteString o r
hPutStr h = fix \go -> (>> go) (Eff.effBase . BS.hPutStr h =<< Pipe.await)

hPutStr' :: (
	Union.Member Pipe.P es,
	Union.Base IO.I es ) =>
	Handle -> Eff.E es BS.ByteString o ()
hPutStr' h = fix \go -> do
	m <- Pipe.isMore
	if m
	then (>> go) do
		Eff.effBase . BS.hPutStr h =<< Pipe.await
	else pure ()
