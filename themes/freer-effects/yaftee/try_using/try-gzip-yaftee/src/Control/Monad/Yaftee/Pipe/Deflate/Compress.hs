{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Deflate.Compress (

	run_, States, compress, Members

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS

import Pipe.RunLength.Compress qualified as RunLength
import Data.Gzip.Block

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor

import GHC.Stack

run_ :: forall nm es i o a . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void . (flip State.runN PipeB.empty) . RunLength.run_

type States nm = RunLength.States nm `Append` '[State.Named nm PipeB.Queue]

compress :: forall (nm :: Symbol) -> (HasCallStack, U.Member Pipe.P es, Members nm es) =>
	Eff.E es BS.ByteString BS.ByteString ()
compress nm = void $ RunLength.compress nm Pipe.=$= PipeL.bundle' 500 Pipe.=$=
	PipeT.convert'' runLengthsToBits [] Pipe.=$= PipeB.toByteString' nm

type Members nm es =
	(RunLength.Members nm es, U.Member (State.Named nm PipeB.Queue) es)
