{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Writer where

import Control.Arrow
import Data.Typeable
import Data.Monoid

import MyEff
import Free
import TypeLevel

data Writer wr w = Writer wr w deriving Functor

tell :: (Member (Writer wr) es, Typeable wr) => wr -> Eff es ()
tell wr = Free . toUnion $ Writer wr (Pure ())

runWriter :: (Monoid wr, Typeable wr) =>
	Eff (Writer wr :> es) a -> Eff es (a, wr)
runWriter = \case
	Pure x -> Pure (x, mempty)
	Free u -> case fromUnion u of
		Just (Writer wr f') -> second (wr <>) <$> runWriter f'
		Nothing -> Free . fmap runWriter $ castUnion u
