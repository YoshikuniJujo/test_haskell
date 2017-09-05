{-# LANGUAGE LambdaCase, DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
-- {-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Coroutine where

import Data.Typeable

import MyEff
import Free
import TypeLevel

data Yield a v = Yield a (() -> v) deriving Functor

yield :: (Member (Yield a) es, Typeable a) => a -> Eff es ()
yield x = Free . toUnion $ Yield x Pure

data Y r a w
	= Y a (() -> Eff r (Y r a w))
	| Done w

runC :: Typeable a => Eff (Yield a :> es) w -> Eff r (Y r a w)
runC = \case
	Pure x -> Pure (Done x)
	Free u -> case fromUnion u of
		Just (Yield x f') -> Pure (Y x (runC . f'))
		Nothing -> Free . fmap runC $ castUnion u
