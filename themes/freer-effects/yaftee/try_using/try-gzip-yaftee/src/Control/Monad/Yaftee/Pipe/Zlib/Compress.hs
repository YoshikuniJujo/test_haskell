{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Zlib.Compress (

	run, States,
	compress, Members
	
	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Adler32 qualified as Adler32
import Control.Monad.Yaftee.Pipe.Deflate.Compress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State

import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
run = Deflate.run_ . Adler32.run_

type States nm = State.Named nm Adler32.A ': Deflate.States nm

compress :: forall (nm :: Symbol) -> (U.Member Pipe.P es, Members nm es) =>
	Eff.E es BS.ByteString BS.ByteString ()
compress nm = void $ Adler32.adler32' nm Pipe.=$= do
	Pipe.yield "\x78\x9c"
	Deflate.compress nm
	Pipe.yield . BS.fromBitsBE' =<< State.getsN nm Adler32.toWord32

type Members nm es =
	(U.Member (State.Named nm Adler32.A) es, Deflate.Members nm es)
