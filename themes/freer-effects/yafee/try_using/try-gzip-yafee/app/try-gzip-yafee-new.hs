{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
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

import Block

import HuffmanTree
import Pipe.Huffman
import Data.Sequence

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(putStrLn . Prelude.take 1000 . show =<<) . runMyEff $
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
		blocks Pipe.=$= format 100 Pipe.=$=
			crcPipe Pipe.=$= do
				fix \go -> Pipe.await >>= \case
					Nothing -> pure ()
					Just x -> print' x >> go
				compCrc

		print' . crcToByteString =<< State.get

		State.put $ RequestBytes 4
		Just efoo <- Pipe.await
		print' =<< getRight =<< getJust =<< case efoo of
			Left _ -> Pipe.await
			Right _ -> pure $ Just efoo
		print' @Word32 . bsToNum =<< getRight =<< getJust =<< Pipe.await

type MyEff = '[
	State.S Request,
	State.S BitArray,
	State.S (BinTree Int, BinTree Int),
	State.S (Seq Word8),
	State.S ExtraBits,
	State.Named "bits" BitArray,
	State.Named "format" BS.ByteString,
	State.S Crc,
	Except.E String, Fail.F, IO ]

type family TupleL t ts where
	TupleL t '[] = t
	TupleL t (t' ': ts) = TupleL (t, t') ts

runMyEff :: Eff.E (Pipe.P () () ': MyEff) a -> IO
	(Either String (Either String (TupleL a '[
		[()], Request, BitArray, (BinTree Int, BinTree Int),
		Seq Word8, ExtraBits, BitArray, BS.ByteString, Crc])))
runMyEff = Eff.runM . Fail.run . Except.run
	. (`State.run` Crc 0) . (`State.runN` "")
	. (`State.runN` byteStringToBitArray "")
	. (`State.run` ExtraBits 0) . (`State.run` empty)
	. (`State.run` (fixedTable, fixedTable))
	. (`State.run` byteStringToBitArray "")
	. (`State.run` RequestBytes 0) . Pipe.run

print' :: (Show a, Union.Member IO effs) => a -> Eff.E effs ()
print' = Eff.eff . print

putStrLn' :: Union.Member IO effs => String -> Eff.E effs ()
putStrLn' = Eff.eff . putStrLn
