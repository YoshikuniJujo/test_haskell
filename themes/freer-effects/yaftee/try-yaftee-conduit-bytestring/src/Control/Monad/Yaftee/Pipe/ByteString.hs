{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString (

	-- * PACKAGE NAME

	Pkg,

	-- * STANDARD INPUT/OUTPUT

	putStr, putStr',

	-- * HANDLE

	hGet, hGet', hPutStr, hPutStr',

	-- * LENGTH

	length, length', Length, lengthToByteString

	) where

import Prelude hiding (putStr, length)
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Bool
import Data.ByteString qualified as BS
import System.IO hiding (putStr, hPutStr)

type Pkg = "try-yaftee-conduit-bytestring"

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

putStr' :: (U.Member Pipe.P es, U.Base IO.I es) => Eff.E es BS.ByteString o ()
putStr' = fix \go -> Pipe.isMore >>= bool (pure ())
	(Pipe.await >>= Eff.effBase . BS.putStr >> go)

hPutStr :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Eff.E es BS.ByteString o ()
hPutStr h = fix \go -> (>> go) $ Eff.effBase . BS.hPutStr h =<< Pipe.await

hPutStr' :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Eff.E es BS.ByteString o ()
hPutStr' h = fix \go -> Pipe.isMore >>= bool (pure ())
	((>> go) $ Eff.effBase . BS.hPutStr h =<< Pipe.await)

length :: (U.Member Pipe.P es, U.Member (State.Named Pkg Length) es) =>
	Eff.E es BS.ByteString BS.ByteString r
length = fix \go -> Pipe.await >>= \bs ->
	State.modifyN Pkg (+ Length (BS.length bs)) >> Pipe.yield bs >> go

length' :: (U.Member Pipe.P es, U.Member (State.Named Pkg Length) es) =>
	Eff.E es BS.ByteString BS.ByteString ()
length' = fix \go -> Pipe.isMore >>= bool (pure ())
	((>> go) $ Pipe.await >>= \bs ->
		State.modifyN Pkg (+ Length (BS.length bs)) >> Pipe.yield bs)

newtype Length = Length { unLength :: Int }
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

lengthToByteString :: Length -> BS.ByteString
lengthToByteString = BS.pack . go (4 :: Int) . unLength
	where
	go n _ | n < 1 = []
	go n ln = fromIntegral ln : go (n - 1) (ln `shiftR` 8)
