{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.ST where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Reader qualified as Reader
import Control.Monad.Yaftee.ST qualified as ST
import Control.HigherOpenUnion qualified as U
import Data.STRef

run :: Eff.E '[Reader.R a, ST.S s] i o r -> a -> ST s r
run m d = Eff.runM $ Reader.run m d

increase :: (U.Member (Reader.R Int) es, U.Base (ST.S s) es) =>
	STRef s Int -> Eff.E es i o ()
increase r = do
	d <- Reader.ask
	ST.modifyRef' r (+ d)

increaseNTimes :: forall s ->
	(U.Member (Reader.R Int) es, U.Base (ST.S s) es) =>
	Int -> Int -> Eff.E es i o Int
increaseNTimes s n x0 = do
	r <- ST.newRef @s x0
	replicateM_ n (increase r)
	ST.readRef r

sample :: forall s . ST s Int
sample = run @Int (increaseNTimes s 3 5) 2
