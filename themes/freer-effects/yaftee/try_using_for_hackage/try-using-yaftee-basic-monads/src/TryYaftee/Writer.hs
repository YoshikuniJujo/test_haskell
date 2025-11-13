{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Writer where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Writer qualified as Writer
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

action :: IO ((), [String])
action = run @[String] getLines

run :: Monoid w => Eff.E '[Writer.W w, IO.I] i o r -> IO (r, w)
run = Eff.runM . Writer.run

getLines :: (U.Member (Writer.W [String]) es, U.Base IO.I es) => Eff.E es i o ()
getLines = IO.getLine >>= \ln ->
	when (not $ null ln) (Writer.tell [ln] >> getLines)
