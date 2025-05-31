{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Lazy (
	hGet, hGet', hPutStr, hPutStr'
	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.IO (Handle, hIsEOF)

hGet :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Int -> Handle -> Eff.E es i LBS.ByteString ()
hGet bfsz h = fix \go ->  Eff.effBase (not <$> hIsEOF h) >>= bool (pure ())
	(Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield . LBS.fromStrict >> go)

hGet' :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Int -> Handle -> Eff.E es i (Maybe LBS.ByteString) ()
hGet' bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>= bool
	(Pipe.yield Nothing)
	(Eff.effBase (BS.hGetSome h bfsz)
		>>= Pipe.yield . Just . LBS.fromStrict >> go)

hPutStr :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Eff.E es LBS.ByteString o r
hPutStr h = fix \go -> (>> go) $ Eff.effBase . LBS.hPut h =<< Pipe.await

hPutStr' :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Eff.E es LBS.ByteString o ()
hPutStr' h = fix \go -> Pipe.isMore  >>= bool (pure ())
	((>> go) $ Eff.effBase . LBS.hPut h =<< Pipe.await)
