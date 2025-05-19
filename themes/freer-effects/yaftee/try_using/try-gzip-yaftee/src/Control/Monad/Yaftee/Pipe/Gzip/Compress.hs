{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Compress (

	run_, States,

	compress, Members

	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS

import Pipe.RunLength.Compress qualified as RunLength
import Data.Gzip.GzipHeader
import Data.Gzip.Block

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor

run_ :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void
	. (flip State.runN PipeB.empty)
	. PipeBS.lengthRun . PipeCrc.runCrc32 . RunLength.run_

type States nm = RunLength.States nm `Append` '[
	State.Named nm PipeCrc.Crc32,
	State.Named nm PipeBS.Length,
	State.Named nm PipeB.Queue  ]

compress :: forall nm -> (U.Member Pipe.P es, Members nm es) =>
	Eff.E es BS.ByteString BS.ByteString ()
compress nm = void $
	PipeCrc.crc32' nm Pipe.=$= PipeBS.length' nm Pipe.=$=
	RunLength.compress nm Pipe.=$= PipeL.bundle' 500 Pipe.=$=
	PipeT.convert'' runLengthsToBits [] Pipe.=$= do
		Pipe.yield $ encodeGzipHeader sampleGzipHeader
		PipeB.toByteString' nm
		PipeCrc.compCrc32 nm
		Pipe.yield . PipeCrc.crc32ToByteString =<< State.getN nm
		Pipe.yield . PipeBS.lengthToByteString =<< State.getN nm

type Members nm es = (
	U.Member (State.Named nm PipeCrc.Crc32) es,
	U.Member (State.Named nm PipeBS.Length) es,
	RunLength.Members nm es,
	U.Member (State.Named nm PipeB.Queue) es )
