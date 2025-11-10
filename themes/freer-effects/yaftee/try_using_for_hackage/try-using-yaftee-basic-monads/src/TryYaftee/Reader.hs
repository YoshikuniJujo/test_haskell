{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Reader where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Reader qualified as Reader
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

run :: Monad m => Eff.E '[Reader.R Int, U.FromFirst m] i o a -> m a
run = Eff.runM . (`Reader.run` 123)

sample :: (U.Member (Reader.R Int) es, U.Base IO.I es) => Eff.E es i o ()
sample = do
	e <- Reader.ask @Int
	IO.print e
	Reader.local @Int (* 2) do
		e' <- Reader.ask @Int
		IO.print e'
