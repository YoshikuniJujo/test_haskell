{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eff (
	Eff, Freer(..), Member, run, runM, tsingleton, qApp, qComp, decomp, inj
	) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Bool (bool)

import Freer (Freer(..), tsingleton, qApp, qComp)
import OpenUnion (Union, Member, inj, decomp, extract)

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "MyEff.run: expect Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Bind u q) = runM . (q `qApp`) =<< extract u

send :: Member eff effs => eff a -> Eff effs a
send = (`Bind` tsingleton Pure) . inj

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance Member NonDet effs => MonadPlus (Eff effs)

instance Member NonDet effs => Alternative (Eff effs) where
	empty = send MZero
	(<|>) = ((send MPlus >>=) .) . bool
