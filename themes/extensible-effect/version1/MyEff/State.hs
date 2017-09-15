{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.State where

import Data.Typeable

import MyEff
import Free
import TypeLevel

data State s w = State (s -> s) (s -> w) deriving Functor

getModify :: (Member (State s) es, Typeable s) => (s -> s) -> Eff es s
getModify f = Free . toUnion $ State f Pure

get :: (Member (State s) es, Typeable s) => Eff es s
get = getModify id

modify :: (Member (State s) es, Typeable s) => (s -> s) -> Eff es ()
modify = (>> return ()) . getModify

put :: (Member (State s) es, Typeable s) => s -> Eff es ()
put = modify . const

runState :: Typeable s => Eff (State s :> es) a -> s -> Eff es (a, s)
runState f s = case f of
	Pure x -> Pure (x, s)
	Free u -> case fromUnion u of
		Just (State m f') -> runState (f' s) (m s)
		Nothing -> Free . fmap (`runState` s) $ castUnion u
