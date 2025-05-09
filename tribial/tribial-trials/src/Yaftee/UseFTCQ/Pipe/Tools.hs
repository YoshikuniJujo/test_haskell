{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Tools where

import Prelude hiding (take)
import Control.Monad.Fix
import Data.Bool
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.IO qualified as IO
import Yaftee.OpenUnion qualified as Union

convert :: Union.Member Pipe.P effs => (a -> b) -> Eff.E effs a b r
convert f = fix \go -> Pipe.await >>= ((>> go) . Pipe.yield . f)

convert' :: Union.Member Pipe.P es => (a -> b) -> Eff.E es a b ()
convert' f = fix \go -> Pipe.isMore
	>>= bool (pure ()) (Pipe.await >>= ((>> go) . Pipe.yield . f))

convert'' :: (
	Union.Member Pipe.P es,
	Union.Base IO.I es -- FOR DEBUG
	) => (Bool -> a -> b) -> a -> Eff.E es a b ()
convert'' f = fix \go p -> Pipe.isMore >>= bool
	(IO.print "BEFORE END" >> Pipe.yield (f True p) >> IO.print "AFTER END")
--	(Pipe.await >>= \x -> ((>> go x) . Pipe.yield $ f False p))
	(Pipe.await >>= \x -> do
		Pipe.yield $ f False p
		IO.print "AFTER NOT END"
		go x)
