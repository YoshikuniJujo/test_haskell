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

newtype Reader r w = Reader (r -> w) deriving Functor

ask :: (Member (Reader r) es, Typeable r) => Eff es r
ask = Free . toUnion $ Reader Pure

runReader :: Typeable r => Eff (Reader r :> es) a -> r -> Eff es a
runReader f r = case f of
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Reader f') -> runReader (f' r) r
		Nothing -> Free . fmap (`runReader` r) $ castUnion u
