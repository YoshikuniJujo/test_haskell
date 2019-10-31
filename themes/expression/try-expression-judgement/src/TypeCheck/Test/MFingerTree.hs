{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.MFingerTree (
	MFingerTree, M(..), MViewL, T.TViewL(..), msingleton, (|>), (><), mviewl
	) where

import qualified TypeCheck.Test.TFingerTree as T

newtype M t a b = M (a -> t b)

type MFingerTree t = T.TFingerTree (M t)
type MViewL t = T.TViewL T.TFingerTree (M t)

msingleton :: (a -> t b) -> MFingerTree t a b
msingleton = T.TSingle . M

(|>) :: MFingerTree t a b -> (b -> t c) -> MFingerTree t a c
(|>) = (. M) . (T.|>)

(><) :: MFingerTree t a b -> MFingerTree t b c -> MFingerTree t a c
(><) = (T.><)

mviewl :: MFingerTree t a b -> MViewL t a b
mviewl = T.viewL
