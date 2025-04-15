{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Crc (

	crcPipe, compCrc,

	Crc(..), crcToByteString,

	) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import Crc

import ByteStringNum

newtype Crc = Crc Word32 deriving Show

crcToByteString :: Crc -> BS.ByteString
crcToByteString (Crc c) = numToBs c

crcPipe :: Union.Member (State.S Crc) effs =>
	Eff.E (Pipe.P BS.ByteString BS.ByteString ': effs) ()
crcPipe = do
	State.put $ Crc 0xffffffff
	crcBody

crcBody :: (
	Union.Member (Pipe.P BS.ByteString BS.ByteString) effs,
	Union.Member (State.S Crc) effs ) =>
	Eff.E effs ()
crcBody = Pipe.await' BS.ByteString >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modify \(Crc c) -> Crc $ c `step'` bs
		Pipe.yield' BS.ByteString (bs :: BS.ByteString)
		crcBody

compCrc :: Union.Member (State.S Crc) effs => Eff.E effs ()
compCrc = State.modify \(Crc c) -> Crc $ complement c
