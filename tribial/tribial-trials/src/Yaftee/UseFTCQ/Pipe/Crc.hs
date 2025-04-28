{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Crc (

	crcPipe, compCrc,

	Crc(..), crcToByteString,

	foobar,

	) where

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.OpenUnion qualified as Union
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import Data.Crc

import Tools.ByteStringNum

newtype Crc = Crc Word32 deriving Show

crcToByteString :: Crc -> BS.ByteString
crcToByteString (Crc c) = numToBs c

crcPipe :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Crc) effs
	) =>
	Eff.E effs BS.ByteString BS.ByteString ()
crcPipe = do
--	State.put $ Crc 0xffffffff
	crcBody

foobar :: Union.Member Pipe.P effs => Eff.E effs a a ()
foobar = (Pipe.yield =<< Pipe.await) >> foobar

crcBody :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Crc) effs ) =>
	Eff.E effs BS.ByteString BS.ByteString ()
crcBody = Pipe.await >>= \case
	bs -> do
--		State.modify \(Crc c) -> Crc $ c `step'` bs
		Pipe.yield (bs :: BS.ByteString)
		crcBody

compCrc :: Union.Member (State.S Crc) effs => Eff.E effs i o ()
compCrc = State.modify \(Crc c) -> Crc $ complement c
