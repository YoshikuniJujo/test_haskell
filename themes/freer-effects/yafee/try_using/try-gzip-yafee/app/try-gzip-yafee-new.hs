{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
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
		. (`State.run` byteStringToBitArray "")
		. (`State.run` RequestBytes 0)
		. (Pipe.run @() @()) $
		fromHandle @(Pipe () BS.ByteString MyEff) (type ()) h Pipe.=$=
		onDemand @MyEff Pipe.=$= do
			checkRight @(Pipe (Either BitArray BS.ByteString) BS.ByteString MyEff) (type BitArray) BS.ByteString Pipe.=$=
				crcPipe @(Pipe BS.ByteString BS.ByteString MyEff) Pipe.=$= do
				State.put $ RequestBytes 2
				Just ids <- Pipe.await @BS.ByteString (type ())
				when (ids /= "\31\139") $ Except.throw @String "Bad magic"
				State.put $ RequestBytes 1
				Just cm <- Pipe.await (type ())
				print' $ CompressionMethod $ BS.head cm
				Just flgs <- (readFlags . BS.head =<<) <$> Pipe.await (type ())
				print' flgs
				State.put $ RequestBytes 4
				Just mtm <- Pipe.await @BS.ByteString (type ())
				print' . posixSecondsToUTCTime . realToFrac . word32ToCTime $ bsToNum mtm
				State.put $ RequestBytes 1
				print' =<< Pipe.await @BS.ByteString (type ())
				print' . (OS . BS.head <$>) =<< Pipe.await @BS.ByteString (type ())
				State.put $ RequestBytes 2
				Just xlen <- (bsToWord16 <$>) <$> Pipe.await @BS.ByteString (type ())
				State.put . RequestBytes $ fromIntegral xlen
				when (flagsRawExtra flgs)
					$ print' . (decodeExtraFields <$>) =<< Pipe.await @BS.ByteString (type ())
				State.put RequestString
				when (flagsRawName flgs)
					$ print' =<< Pipe.await @BS.ByteString (type ())
				when (flagsRawComment flgs)
					$ print' =<< Pipe.await @BS.ByteString (type ())
				when (flagsRawHcrc flgs) do
					compCrc
					putStrLn' . (`showHex` "") . (.&. 0xffff) . (\(Crc c) -> c) =<< State.get @Crc
					State.put $ RequestBytes 2
					maybe (pure ()) putStrLn' . (((`showHex` "") . bsToNum @Word16) <$>) =<< Pipe.await @BS.ByteString (type ())
			State.put $ RequestBits 1
			print' . (either (Left . bitArrayToWord8) Right <$>) =<< Pipe.await @(Either BitArray BS.ByteString) (type ())
			State.put $ RequestBits 2
			bt <- bitArrayToWord8 <$> (getLeft =<< getJust =<< Pipe.await @(Either BitArray BS.ByteString) (type ()))
			print' bt
			case bt of
				0 -> do		State.put $ RequestBytes 4
						ln <- getWord16FromPair =<< skipLeft1 BS.ByteString (type BitArray) BS.ByteString
						State.put $ RequestBytes $ fromIntegral ln
						(Pipe.yield (Either (type BitArray) BS.ByteString) =<< getRight =<< getJust =<< Pipe.await @(Either BitArray BS.ByteString) BS.ByteString)
						State.put $ RequestBytes 4
						(Pipe.yield (Either (type BitArray) BS.ByteString) =<< getRight =<< getJust =<< Pipe.await @(Either BitArray BS.ByteString) BS.ByteString)
						State.put $ RequestBytes 4
						(Pipe.yield (Either (type BitArray) BS.ByteString) =<< getRight =<< getJust =<< Pipe.await @(Either BitArray BS.ByteString) BS.ByteString)
					Pipe.=$= do	crcPipe @(Pipe BS.ByteString BS.ByteString MyEff)
					Pipe.=$= do
						print' =<< Pipe.await @BS.ByteString (type ()) :: Eff.E (Pipe BS.ByteString () MyEff) ()
						compCrc
						print' . crcToByteString =<< State.get
						print' =<< Pipe.await @BS.ByteString (type ())
						print' =<< Pipe.await @BS.ByteString (type ())
				1 -> do	State.put $ RequestBuffer 100
					pure ()
				2 -> do	State.put $ RequestBuffer 100
					pure ()
				_ -> Except.throw $ "No such BTYPE: " ++ show bt

getWord16FromPair :: Union.Member (Except.E String) effs =>
	BS.ByteString -> Eff.E effs Word16
getWord16FromPair bs = do
	when (BS.length bs /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where
	(ln, cln) = (tow16 *** tow16) $ BS.splitAt 2 bs
	tow16 bs = let [b0, b1] = BS.unpack bs in
		fromIntegral b0 .|. (fromIntegral b1) `shiftL` 8

skipLeft1 :: forall effs . forall o -> forall a -> forall b -> (
	Union.Member (Pipe.P (Either a b) o) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E effs b
skipLeft1 o a b = Pipe.await @(Either a b) o >>= \case
	Just (Left _) -> Pipe.await @(Either a b) o >>= \case
		Just (Left _) -> Except.throw @String "Not Right"
		Just (Right x) -> pure x
		Nothing -> Except.throw @String "Not enough input"
	Just (Right x) -> pure x
	Nothing -> Except.throw @String "Not enough input"

getJust :: Union.Member (Except.E String) effs => Maybe a -> Eff.E effs a
getJust = \case
	Nothing -> Except.throw @String "Not Just"
	Just x -> pure x

getLeft :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs a
getLeft = \case
	Left x -> pure x
	Right _ -> Except.throw @String "Not Left"

getRight :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs b
getRight = \case
	Left _ -> Except.throw @String "No Right"
	Right x -> pure x

print' :: (Show a, Union.Member IO effs) => a -> Eff.E effs ()
print' = Eff.eff . print

putStrLn' :: Union.Member IO effs => String -> Eff.E effs ()
putStrLn' = Eff.eff . putStrLn

type Pipe i o effs = Pipe.P i o ': effs
type MyEff = '[
	State.S Request,
	State.S BitArray,
	State.S Crc,
	Except.E String, Fail.F, IO]
