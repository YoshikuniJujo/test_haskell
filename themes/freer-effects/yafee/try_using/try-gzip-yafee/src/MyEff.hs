{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Data.Sequence
import Data.Word
import Data.ByteString qualified as BS

import BitArray qualified as BitArray
import Pipe.ByteString.OnDemand
import Pipe.Huffman
import Pipe.Crc
import HuffmanTree

type MyEff = '[
	State.S Request,
	State.S BitArray.B,
	State.S (BinTree Int, BinTree Int),
	State.S (Seq Word8),
	State.S ExtraBits,
	State.Named "bits" BitArray.B,
	State.Named "format" BS.ByteString,
	State.S Crc,
	Except.E String, Fail.F, IO ]

runMyEff :: Eff.E (Pipe.P () () ': MyEff) a -> IO
	(Either String (Either String (TupleL a '[
		[()], Request, BitArray.B, (BinTree Int, BinTree Int),
		Seq Word8, ExtraBits, BitArray.B, BS.ByteString, Crc])))
runMyEff = Eff.runM . Fail.run . Except.run
	. (`State.run` Crc 0) . (`State.runN` "")
	. (`State.runN` BitArray.empty)
	. (`State.run` ExtraBits 0) . (`State.run` empty)
	. (`State.run` (fixedTable, fixedTable))
	. (`State.run` BitArray.empty)
	. (`State.run` RequestBytes 0) . Pipe.run

type family TupleL t ts where
	TupleL t '[] = t; TupleL t (t' ': ts) = TupleL (t, t') ts
