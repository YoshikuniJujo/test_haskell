{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.FTCQueue (
	-- * FTCQueue
	FTCQueue ) where

import Control.Arrow ((>>>))
import Control.Monad.Freer.Par.Sequence (Sequence(..), ViewL(..))

---------------------------------------------------------------------------

data FTCQueue cat a b where
	Empty :: FTCQueue cat a a
	Node :: FTCQueue cat a b ->
		cat b c -> FTCQueue cat c d -> FTCQueue cat a d

instance Sequence FTCQueue where
	empty = Empty; singleton x = Node Empty x Empty
	(><) l = viewl >>> \case EmptyL -> l; x :<| r -> Node l x r
	viewl = \case Empty -> EmptyL; Node l x r -> vwl l x r

vwl :: FTCQueue cat a b -> cat b c -> FTCQueue cat c d -> ViewL FTCQueue cat a d
vwl Empty x r = x :<| r; vwl (Node ll x lr) y r = vwl ll x $ Node lr y r
