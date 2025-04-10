{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.IO
import System.Environment

import Pipe.ByteString.IO
import Pipe.ByteString.OnDemand
import Pipe.Crc
import Pipe.DataCheck

import Gzip
import ByteStringNum
import Numeric

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(print =<<) . Eff.runM . Fail.run . Except.run
		. (`State.run` Crc 0)
		. (`State.run` "")
		. (`State.run` RequestBytes 0)
		. (Pipe.run @() @()) $
		fromHandle @(Pipe () BS.ByteString MyEff) (type ()) h Pipe.=$=
		onDemand @(Pipe BS.ByteString (Either BitArray BS.ByteString) MyEff) Pipe.=$= do
			checkRight @(Pipe (Either BitArray BS.ByteString) BS.ByteString MyEff) (type BitArray) BS.ByteString Pipe.=$=
				crcPipe @(Pipe BS.ByteString BS.ByteString MyEff) Pipe.=$= do
				State.put $ RequestBytes 2
				Just ids <- Pipe.await @BS.ByteString (type ())
				when (ids /= "\31\139") $ Except.throw @String "Bad magic"
				State.put $ RequestBytes 1
				Just cm <- Pipe.await (type ())
				print' $ CompressionMethod $ BS.head cm
				Just flgs <- Pipe.await (type ())
				print' . readFlags $ BS.head flgs
				State.put $ RequestBytes 4
				Just mtm <- Pipe.await @BS.ByteString (type ())
				print' . posixSecondsToUTCTime . realToFrac . word32ToCTime $ bsToNum mtm
				State.put $ RequestBytes 1
				print' =<< Pipe.await @BS.ByteString (type ())
				print' . (OS . BS.head <$>) =<< Pipe.await @BS.ByteString (type ())
				State.put $ RequestBytes 2
				Just xlen <- (bsToWord16 <$>) <$> Pipe.await @BS.ByteString (type ())
				State.put . RequestBytes $ fromIntegral xlen
				print' . (decodeExtraFields <$>) =<< Pipe.await @BS.ByteString (type ())
				State.put RequestString
				print' =<< Pipe.await @BS.ByteString (type ())
				print' =<< Pipe.await @BS.ByteString (type ())
				compCrc
				putStrLn' . (`showHex` "") . (.&. 0xffff) . (\(Crc c) -> c) =<< State.get @Crc
				State.put $ RequestBytes 2
				maybe (pure ()) putStrLn' . (((`showHex` "") . bsToNum @Word16) <$>) =<< Pipe.await @BS.ByteString (type ())
			State.put $ RequestBuffer 20
			print' =<< Pipe.await @(Either BitArray BS.ByteString) (type ())
			print' =<< Pipe.await @(Either BitArray BS.ByteString) (type ())
			print' =<< Pipe.await @(Either BitArray BS.ByteString) (type ())

print' :: (Show a, Union.Member IO effs) => a -> Eff.E effs ()
print' = Eff.eff . print

putStrLn' :: Union.Member IO effs => String -> Eff.E effs ()
putStrLn' = Eff.eff . putStrLn

type Pipe i o effs = Pipe.P i o ': effs
type MyEff = '[
	State.S Request,
	State.S BS.ByteString,
	State.S Crc,
	Except.E String, Fail.F, IO]
