{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React where

import Data.Set

import Freer
import FTCQueue
import TaggableFunction

type EvReqs = Set
type EvOccs = Set

data Rct e a where Await :: EvReqs e -> Rct e (EvOccs e)

type React s e a = Freer s FTCQueue (Taggable s) (Rct e) a

exper :: EvReqs e -> React s e (EvOccs e)
exper rs = Await rs >>>= pure

interpret :: Monad m => (EvReqs e -> m (EvOccs e)) -> React s e a -> m a
interpret _ (Pure x) = pure x
interpret p (Await e :>>= c) = p e >>= interpret p . qApp c
