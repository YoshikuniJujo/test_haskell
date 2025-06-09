{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Sequence.Adler32 where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Word
import Data.Word.Adler32 qualified as Adler32

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm (Int, Adler32.A) ': es) i o r -> Eff.E es i o (r, (Int, Adler32.A))
run = (`State.runN` Adler32.initial)

adler32 :: forall nm -> (
	Foldable t,
	U.Member Pipe.P es, U.Member (State.Named nm (Int, Adler32.A)) es) =>
	Eff.E es (t Word8) (t Word8) r
adler32 nm = State.putN nm Adler32.initial >> body nm

body :: forall nm -> (
	Foldable t,
	U.Member Pipe.P es, U.Member (State.Named nm (Int, Adler32.A)) es) =>
	Eff.E es (t Word8) (t Word8) r
body nm = forever $ Pipe.await >>= \s -> do
	State.modifyN nm (`step` s)
	Pipe.yield s

step :: Foldable t => (Int, Adler32.A) -> t Word8 -> (Int, Adler32.A)
step = foldl Adler32.step
