{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Reader where

import Data.Typeable

import Free
import MyEff
import TypeLevel

newtype Reader r w = Reader { unReader :: r -> w } deriving Functor

ask :: (Member (Reader r) es, Typeable r) => Eff es r
ask = Free . toUnion $ Reader Pure

runReader :: Typeable r => Eff (Reader r :> es) a -> r -> Eff es a
runReader f r = mkRun id (const id) (\f' -> unReader f' r) f
