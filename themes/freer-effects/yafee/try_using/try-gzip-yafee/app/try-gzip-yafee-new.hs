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
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Foldable
import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.Time.Clock()
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

import BitArray(BitArray(..))

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(print =<<) . Eff.runM . Fail.run . Except.run
		. (`State.run` Crc 0) . (`State.run` byteStringToBitArray "")
		. (`State.run` RequestBytes 0) . (Pipe.run @() @()) $
		fromHandle h Pipe.=$= onDemand @MyEff Pipe.=$= do
		checkRight Pipe.=$= crcPipe Pipe.=$= do
			State.put $ RequestBytes 2
			Just ids <- Pipe.await
			when (ids /= "\31\139")
				$ Except.throw @String "Bad magic"
			State.put $ RequestBytes 1
			Just cm <- Pipe.await
			print' $ CompressionMethod $ BS.head cm
			Just flgs <- (readFlags . BS.head =<<) <$> Pipe.await
			print' flgs
			State.put $ RequestBytes 4
			Just mtm <- Pipe.await @BS.ByteString
			print' . posixSecondsToUTCTime
				. realToFrac . word32ToCTime $ bsToNum mtm
			State.put $ RequestBytes 1
			print' =<< Pipe.await @BS.ByteString
			print' . (OS . BS.head <$>)
				=<< Pipe.await @BS.ByteString
			when (flagsRawExtra flgs) do
				State.put $ RequestBytes 2
				Just xlen <-
					(bsToWord16 <$>) <$> Pipe.await @BS.ByteString
				State.put . RequestBytes $ fromIntegral xlen
				print' . (decodeExtraFields <$>) =<< Pipe.await
			State.put RequestString
			when (flagsRawName flgs)
				$ print' =<< Pipe.await @BS.ByteString
			when (flagsRawComment flgs)
				$ print' =<< Pipe.await @BS.ByteString
			when (flagsRawHcrc flgs) do
				compCrc
				putStrLn' . (`showHex` "") . (.&. 0xffff)
					. (\(Crc c) -> c) =<< State.get @Crc
				State.put $ RequestBytes 2
				maybe (pure ()) putStrLn'
					. (((`showHex` "") . bsToNum @Word16) <$>)
					=<< Pipe.await @BS.ByteString
		(fix \go ->block >>= bool (pure ()) go) Pipe.=$=
			crcPipe Pipe.=$= do
				fix \go -> Pipe.await >>= \case
					Nothing -> pure ()
					Just x -> print' x >> go
				compCrc

		print' . crcToByteString =<< State.get
		State.put $ RequestBytes 4
		print' =<< getRight =<< getJust =<< Pipe.await
		print' @Word32 . bsToNum =<< getRight =<< getJust =<< Pipe.await

block :: (
	Union.Member (State.S Request) effs,
	Union.Member (Except.E String) effs, Union.Member Fail.F effs ) =>
	Eff.E (Pipe.P (Either BitArray BS.ByteString) BS.ByteString ': effs) Bool
block = do
	State.put $ RequestBits 1
	Just (Left t) <- (either (Left . bitArrayToWord8) Right <$>)
		<$> Pipe.await @(Either BitArray BS.ByteString)
	State.put $ RequestBits 2
	bt <- bitArrayToWord8 <$> (
		getLeftJust =<< Pipe.await @(Either BitArray BS.ByteString) )
	case bt of
		0 -> do	State.put $ RequestBytes 4
			ln <- getWord16FromPair =<< skipLeft1
			for_ (separate 10 ln) \ln' -> do
				State.put $ RequestBytes ln'
				Pipe.yield =<< getRightJust =<< Pipe.await
		1 -> do	State.put $ RequestBuffer 100
			pure ()
		2 -> do	State.put $ RequestBuffer 100
			pure ()
		_ -> Except.throw $ "No such BTYPE: " ++ show bt
	pure $ t /= 1

separate :: Int -> Int -> [Int]
separate bs ln
	| ln == 0 = [] | ln <= bs = [ln]
	| otherwise = bs : separate bs (ln - bs)

getWord16FromPair :: (
	Union.Member (Except.E String) effs,
	Integral n
	) =>
	BS.ByteString -> Eff.E effs n
getWord16FromPair bs0 = fromIntegral @Word16 <$> do
	when (BS.length bs0 /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where
	(ln, cln) = (tow16 *** tow16) $ BS.splitAt 2 bs0
	tow16 bs = case BS.unpack bs of
		[b0, b1] -> fromIntegral b0 .|. (fromIntegral b1) `shiftL` 8
		_ -> error "never occur"

skipLeft1 :: -- forall effs . forall o -> forall a -> forall b -> (
	Union.Member (Except.E String) effs =>
	Eff.E (Pipe.P (Either a b) o ': effs) b
skipLeft1 = Pipe.await >>= \case
	Just (Left _) -> Pipe.await >>= \case
		Just (Left _) -> Except.throw @String "Not Right"
		Just (Right x) -> pure x
		Nothing -> Except.throw @String "Not enough input"
	Just (Right x) -> pure x
	Nothing -> Except.throw @String "Not enough input"

getJust :: Union.Member (Except.E String) effs => Maybe a -> Eff.E effs a
getJust = \case
	Nothing -> Except.throw @String "Not Just"
	Just x -> pure x

getLeftJust :: Union.Member (Except.E String) effs =>
	Maybe (Either a b) -> Eff.E effs a
getLeftJust = getLeft <=< getJust

getLeft :: Union.Member (Except.E String) effs => Either a b -> Eff.E effs a
getLeft = \case
	Left x -> pure x
	Right _ -> Except.throw @String "Not Left"

getRightJust ::
	Union.Member (Except.E String) effs =>
	Maybe (Either a b) -> Eff.E effs b
getRightJust = getRight <=< getJust

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
