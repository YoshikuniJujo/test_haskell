{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UniqueMonad (CountT, runCountT, TaggedMonad, mkMonad, parMonad, apply) where

import UniqueMonad.Internal
