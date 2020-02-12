{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TaggedFTCQueue (taggedMonadToExp) where

import UniqueMonad.Internal
import FTCQueue

taggedMonadToExp :: TaggedMonad s m a b -> TaggedExp s m a b
taggedMonadToExp (TaggedMonad t f) = tsingleton $ Tagged t f
