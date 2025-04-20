{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module CheckRawDeflate where

import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.Monad.Yafee.IO qualified as YafeeIO
import Control.OpenUnion qualified as Union
import Data.Sequence
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import BitArray qualified as BitArray
import Pipe.ByteString.OnDemand
import Pipe.Crc
import Pipe.Huffman
import HuffmanTree

import ByteStringNum
import Block
import MyEff

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(putStrLn . Prelude.take 1000 . show =<<) . runMyEff $
		PipeBS.hGet 100 h Pipe.=$= gzipPipe

gzipPipe :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray.B) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.S (Seq Word8)) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (State.Named "format" BS.ByteString) effs,

	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,

	Union.Base IO effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) ()
gzipPipe = onDemand Pipe.=$= do

	blocks Pipe.=$= format 100 Pipe.=$= crcPipe Pipe.=$= do
		fix \go -> Pipe.await >>= \case
			Nothing -> pure ()
			Just x -> YafeeIO.print x >> go
		compCrc

{-
	YafeeIO.print . crcToByteString =<< State.get

	State.put $ RequestBytes 4
	Just efoo <- Pipe.await
	YafeeIO.print =<< getRightJust =<< case efoo of
		Left _ -> Pipe.await
		Right _ -> pure $ Just efoo
	YafeeIO.print @Word32 . bsToNum =<< getRightJust =<< Pipe.await
	-}
