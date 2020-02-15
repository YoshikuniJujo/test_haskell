{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.FTCQueue.SimpleZero where

data FTCQueue a = Empty | Node (FTCQueue a) a (FTCQueue a) deriving Show

viewl :: FTCQueue a -> Maybe (a, FTCQueue a)
viewl Empty = Nothing
viewl (Node l0 x0 r0) = Just $ go l0 x0 r0
	where
	go Empty x r = (x, r)
	go (Node ll x lr) y r = go ll x (Node lr y r)
