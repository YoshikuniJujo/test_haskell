{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.ArrowLike where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

swap :: U.Member Pipe.P es => Eff.E es (a, b) (b, a) r
swap = forever $ Pipe.yield . (\(x, y) -> (y, x)) =<< Pipe.await

-- first :: U.Member Pipe.P es => Eff.E es i o r -> Eff.E es (i, a) (o, a) r
-- first p = forever 

first' :: U.Member Pipe.P es => Eff.E es i o r ->
	Eff.E es (i, a) o (Eff.E es (i, a) i r0, Eff.E es i o r)
first' p = (forever $ Pipe.yield . (\(x, y) -> x) =<< Pipe.await) Pipe.=$= p

push :: U.Member (State.S [a]) es => a -> Eff.E es i o ()
push a = State.modify (a :)
