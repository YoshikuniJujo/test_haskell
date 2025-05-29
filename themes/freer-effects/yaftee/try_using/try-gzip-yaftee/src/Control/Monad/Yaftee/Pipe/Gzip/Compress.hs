{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Compress (

	run_, States, compress, Members

	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS

import Data.Gzip.GzipHeader

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor

import Control.Monad.Yaftee.Pipe.Deflate.Compress qualified as Deflate

run_ :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void . PipeBS.lengthRun . PipeCrc.runCrc32 . Deflate.run_

type States nm = Deflate.States nm `Append`
	'[State.Named nm PipeCrc.Crc32, State.Named nm PipeBS.Length]

compress :: forall nm -> (U.Member Pipe.P es, Members nm es) =>
	Eff.E es BS.ByteString BS.ByteString ()
compress nm = void $ do
	Pipe.yield $ encodeGzipHeader sampleGzipHeader
	_ <- PipeCrc.crc32' nm Pipe.=$= PipeBS.length' nm Pipe.=$= Deflate.compress nm
	PipeCrc.compCrc32 nm
	Pipe.yield . PipeCrc.crc32ToByteString =<< State.getN nm
	Pipe.yield . PipeBS.lengthToByteString =<< State.getN nm

type Members nm es = (
	U.Member (State.Named nm PipeCrc.Crc32) es,
	U.Member (State.Named nm PipeBS.Length) es,
	Deflate.Members nm es )
