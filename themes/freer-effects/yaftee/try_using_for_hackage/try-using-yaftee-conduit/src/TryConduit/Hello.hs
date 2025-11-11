{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryConduit.Hello where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

runHello :: Eff.E '[Pipe.P, IO.I] i o r -> IO (Maybe r)
runHello = Eff.runM . Pipe.run

hello, hello' :: (U.Member Pipe.P es, U.Base IO.I es) => Eff.E es i o ()
hello =	void $
	(Pipe.yield "Hello" >> Pipe.yield "World")
	Pipe.=$=
	PipeIO.print

hello' = void $
	(Pipe.yield "Hello" >> Pipe.yield "World")
	Pipe.=$=
	(Pipe.await >>= IO.print >> Pipe.await >>= IO.print)
